

module SMACCMPilot.Datalink.Client.Loopback where

import Control.Monad
import Control.Concurrent (threadDelay)
import Text.Printf
import System.Random

import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.ByteString
import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Datalink.Client

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B


checkLoopback :: Console
              -> [ByteString] -- Frames to send
              -> Pushable ByteString
              -> Poppable ByteString
              -> Int -- delay between frames, in milliseconds
              -> IO ()
checkLoopback console inputs in_q out_q d = do
  consoleLog console (printf "Checking loopback: %d frames, %d ms betwen frames" (length inputs) d)
  forM_ os $ \(_ix, fc) -> do
    threadDelay (1000*d)
    consoleLog console ("sending: " ++ bytestringShowHex fc)
    queuePush in_q fc

  -- give time for everything to be received.
  threadDelay (4000*d)

  forM_ os $ \(ix, fc) -> do
    p <- queueTryPop out_q
    case p of
      Nothing -> consoleLog console (printf "no response for frame %d" ix)
      Just fc' -> case fc' == fc of
        False -> consoleLog console
          (printf ("incorrect response for frame %d:\n expected %s\ngot %s")
                  ix
                  (bytestringShowHex fc)
                  (bytestringShowHex fc'))
        True ->  consoleLog console (printf "got expected result for frame %d" ix)
  threadDelay 1000000 -- magic, stops app from crashing???
  consoleLog console "Done. Use ^C to exit."
  where
  os :: [(Int, ByteString)]
  os = zip [0..] inputs

randomBytestring :: Integer -> IO ByteString
randomBytestring len = do
  bs <- replicateM (fromIntegral len) randomIO
  return (B.pack bs)

frameLoopbackClient :: Options -> IO ()
frameLoopbackClient opts = datalinkClient opts PlaintextMode run
  where
  run to fro console = do
    cts <- replicateM 20 (randomBytestring plaintextSize)
    checkLoopback console cts to fro 100

commsecLoopbackClient :: Options -> SymmetricKey -> IO ()
commsecLoopbackClient opts sk = datalinkClient opts (SymmetricCommsecMode DatalinkClient sk) run
  where
  run to fro console = do
    cts <- replicateM 20 (randomBytestring plaintextSize)
    checkLoopback console cts to fro 100

