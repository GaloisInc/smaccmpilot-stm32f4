
module SMACCMPilot.Datalink.Client where

import Control.Monad
import Control.Concurrent (threadDelay)
import Text.Printf
import Pipes
import System.Random
import System.IO
import System.Exit

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Pipes
import SMACCMPilot.Datalink.Client.ByteString

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Keys

frameLoopbackClient :: Options -> IO ()
frameLoopbackClient opts = do
  console <- newConsole opts

  (ser_out_push, ser_out_pop) <- newQueue
  (ser_in_push, ser_in_pop)   <- newQueue

  serialServer opts console ser_in_push ser_out_pop

  (out_frame_push, out_frame_pop) <- newQueue
  (in_frame_push, in_frame_pop) <- newQueue

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> untagger 0
        >-> pushConsumer in_frame_push

  a <- asyncRunEffect console "serial out"
           $ popProducer out_frame_pop
         >-> tagger 0
         >-> frameLog
         >-> hxEncoder
         >-> pushConsumer ser_out_push

  cts <- replicateM 20 (randomBytestring cyphertextSize)
  r <- checkLoopback cts out_frame_push in_frame_pop 100
  o <- getConsoleOutput console
  putStrLn o
  case r of
    True -> putStrLn "Success!">> exitSuccess >> return ()
    False -> exitFailure >> return ()
  -- Unreachable - prevents exception that serial in and serial out
  -- are blocked forever
  wait a

commsecLoopbackClient :: Options -> SymmetricKey -> IO ()
commsecLoopbackClient opts sk = do
  console <- newConsole opts

  (ser_out_push, ser_out_pop) <- newQueue
  (ser_in_push, ser_in_pop)   <- newQueue

  serialServer opts console ser_in_push ser_out_pop

  (out_frame_push, out_frame_pop) <- newQueue
  (in_frame_push, in_frame_pop) <- newQueue

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> untagger 0
        >-> commsecDecoder (s2c_ks sk)
        >-> pushConsumer in_frame_push

  a <- asyncRunEffect console "serial out"
           $ popProducer out_frame_pop
         >-> commsecEncoder (c2s_ks sk)
         >-> tagger 0
         >-> frameLog
         >-> hxEncoder
         >-> pushConsumer ser_out_push

  cts <- replicateM 20 (randomBytestring plaintextSize)
  r <- checkLoopback cts out_frame_push in_frame_pop 100
  o <- getConsoleOutput console
  putStrLn o
  case r of
    True -> putStrLn "Success!">> exitSuccess >> return ()
    False -> exitFailure >> return ()

  -- Unreachable - keeps a reference alive in order to prevent exception that
  -- serial in and serial out are STM blocked forever after serialserver closes.
  wait a



checkLoopback :: [ByteString] -- Frames to send
              -> Pushable ByteString
              -> Poppable ByteString
              -> Int -- delay between frames, in milliseconds
              -> IO Bool
checkLoopback inputs in_q out_q d = do
  putStrLn (printf "Checking loopback: %d frames, %d ms betwen frames" (length inputs) d)
  forM_ os $ \(_ix, fc) -> do
    putStrLn ("sending: " ++ bytestringShowHex fc)
    queuePush in_q fc
    threadDelay (1000*d)

  rs <- forM os $ \(ix, fc) -> do
    p <- queueTryPop out_q
    case p of
      Nothing -> hPutStrLn stderr (printf "no response for frame %d" ix)
        >> return False
      Just fc' -> case fc' == fc of
        False -> hPutStrLn stderr
          (printf ("incorrect response for frame %d:\n expected %s\ngot %s")
                  ix
                  (bytestringShowHex fc)
                  (bytestringShowHex fc'))
          >> return False
        True -> return True
  return (and rs)
  where
  os :: [(Int, ByteString)]
  os = zip [0..] inputs

randomBytestring :: Integer -> IO ByteString
randomBytestring len = do
  bs <- replicateM (fromIntegral len) randomIO
  return (B.pack bs)

