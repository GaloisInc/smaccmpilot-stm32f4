
module SMACCMPilot.Comm.Native.Client where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Pipes
import Control.Monad
import Control.Concurrent (threadDelay)

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.Pipes
import SMACCMPilot.Commsec.Sizes

import SMACCMPilot.Comm.Native.Interface.ControllableVehicle

commClient :: Options -> IO ()
commClient opts = do
  console <- newConsole opts

  (ser_out_push, ser_out_pop) <- newQueue
  (ser_in_push, ser_in_pop)   <- newQueue

  serialServer opts console ser_in_push ser_out_pop

  (out_msg_push, out_msg_pop) <- newQueue
  (in_msg_push, in_msg_pop) <- newQueue

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> untagger 0
        >-> msgDeserialize
        >-> pushConsumer in_msg_push

  a <- asyncRunEffect console "serial out"
           $ popProducer out_msg_pop
         >-> msgSerialize
         >-> msgPackFrame
         >-> tagger 0
         >-> frameLog
         >-> hxEncoder
         >-> pushConsumer ser_out_push

  -- XXX do something to produce output to out_msg_push
  -- XXX do something to consume input from in_msg_pop

  void $ forever $ do
    threadDelay 10000
    o <- getConsoleOutput console
    putStrLn o
  -- Unreachable - prevents exception that serial in and serial out
  -- are blocked forever
  wait a


msgDeserialize :: Pipe ByteString ControllableVehicleProducer GW ()
msgDeserialize = forever $ do
  ser <- await
  process ser
  where
  process ser = case runGetState get ser 0 of
    Left e -> error "XXX if contents of ser were nonzero LOG TO THE CONSOLE"
    Right (r, rest) -> do yield r
                          unless (B.null rest) $ process rest

msgSerialize :: Pipe ControllableVehicleProducer ByteString GW ()
msgSerialize = forever $ do
  msg <- await
  yield (runPut (put msg))

msgPackFrame :: Pipe ByteString ByteString GW ()
msgPackFrame = collect B.empty max_len
  where
  max_len = fromIntegral plaintextSize
  collect ms len_rem = do
    m <- await
    let l = B.length m
    if l < len_rem
       then collect (B.append ms m) (len_rem - l)
       else if l > max_len
               then error "XXX HANDLE THIS CASE" >>
                    collect ms len_rem
               else yield ms >>
                    collect m (len_rem - l)

