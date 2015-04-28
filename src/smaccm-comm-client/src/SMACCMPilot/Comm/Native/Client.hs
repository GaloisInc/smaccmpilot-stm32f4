{-# LANGUAGE MultiWayIf #-}

module SMACCMPilot.Comm.Native.Client where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Pipes
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TQueue)

import Test.QuickCheck

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.Pipes
import SMACCMPilot.Commsec.Sizes

import SMACCMPilot.Comm.Native.Interface.ControllableVehicle ()
import qualified SMACCMPilot.Comm.Native.Rpc.ControllableVehicle as RPC

commClient :: Options -> IO ()
commClient opts = do
  console <- newConsole opts

  (ser_in_pop, ser_out_push) <- serialServer opts console

  (out_msg_push, out_msg_pop) <- newQueue
  (in_msg_push, in_msg_pop) <- newQueue

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> untagger 0
        >-> msgDeserialize
        >-> showLog
        >-> pushConsumer in_msg_push

  a <- asyncRunEffect console "serial out"
           $ popProducer out_msg_pop
         >-> msgSerialize
         >-> tagger 0
         >-> frameLog
         >-> hxEncoder
         >-> pushConsumer ser_out_push

  _ <- asyncServer RPC.rpcServer in_msg_pop out_msg_push
                   RPC.Config { RPC.cfgPort = 8080, RPC.cfgStaticDir = Nothing }

  void $ forever $ do
    threadDelay 10000
    o <- getConsoleOutput console
    when (o /= "") (putStrLn o)
  -- Unreachable - prevents exception that serial in and serial out
  -- are blocked forever
  wait a


asyncServer :: (TQueue producer -> TQueue consumer -> RPC.Config -> IO ())
            -> Poppable producer -> Pushable consumer -> RPC.Config -> IO ()
asyncServer s producer consumer conf = void $ asyncRun "asyncServer" $
  s (unPoppable producer) (unPushable consumer)  conf


msgDeserialize :: (Serialize a) => Pipe ByteString a GW ()
msgDeserialize = forever $ do
  ser <- await
  process ser
  where
  process ser = case runGetState get ser 0 of
    Left e -> lift $ writeLog ("deserialize error: " ++ e)
    Right (r, rest) -> do
      yield r
      unless (B.null rest || B.all (== (toEnum 0)) rest) $ process rest

msgSerialize :: (Serialize a) => Pipe a ByteString GW ()
msgSerialize = forever $ do
  msg <- await
  yield (runPut (put msg))


randomMessage :: (Arbitrary a) => Int -> Producer a GW ()
randomMessage delay = forever $ do
  m <- liftIO (generate arbitrary)
  yield m
  liftIO $ threadDelay (delay * 1000)

msgPackFrame :: Pipe ByteString ByteString GW ()
msgPackFrame = aux B.empty max_len
  where
  max_len = fromIntegral plaintextSize
  aux ms len_rem = do
    m <- await
    let l = B.length m
    if | l < len_rem -> aux (B.append ms m) (len_rem - l)
       | l > max_len -> do lift (writeErr e)
                           aux ms len_rem
       | otherwise -> do yield ms
                         aux m (len_rem - l)
  e = "Discarded input to msgPackFrame which was larger than frame size"
