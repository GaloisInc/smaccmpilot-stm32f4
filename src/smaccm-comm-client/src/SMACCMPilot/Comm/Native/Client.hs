{-# LANGUAGE MultiWayIf #-}

module SMACCMPilot.Comm.Native.Client where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Pipes
import Control.Monad
import Control.Concurrent.STM (TQueue)

import SMACCMPilot.Datalink.Client
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Comm.Native.Client.Opts

import SMACCMPilot.Comm.Native.Interface.ControllableVehicle ()
import qualified SMACCMPilot.Comm.Native.Rpc.ControllableVehicle as RPC

commClient :: ClientOptions -> DatalinkMode -> IO ()
commClient clientopts mode = datalinkClient opts mode $ \to fro console -> do

  (out_msg_push, out_msg_pop) <- newQueue
  (in_msg_push, in_msg_pop) <- newQueue

  a <- asyncRunEffect console "deserialize"
          $ popProducer fro
        >-> msgDeserialize
        >-> pushConsumer in_msg_push

  b <- asyncRunEffect console "serialize"
           $ popProducer out_msg_pop
         >-> msgSerialize
         >-> pushConsumer to

  asyncServer RPC.rpcServer in_msg_pop out_msg_push rpccfg
  wait a
  wait b
  where
  opts = dlOpts clientopts
  rpccfg = RPC.Config
    { RPC.cfgPort = srvPort clientopts
    , RPC.cfgStaticDir = Just "./web/"
    }

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

