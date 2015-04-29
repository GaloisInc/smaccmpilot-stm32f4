{-# LANGUAGE ScopedTypeVariables #-}
module SMACCMPilot.Datalink.Client.Serial
  ( serialServer
  ) where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Exception (bracket)

import           System.Exit
import           System.IO
import           System.Serial

import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Async

serialServer :: Options
             -> Console
             -> IO (Poppable ByteString, Pushable ByteString)
serialServer opts console = case serPort opts of
  Nothing -> hPutStrLn stderr "error: must provide a serial port" >> exitFailure
  Just port -> do
    (ser_out_push, ser_out_pop) <- newQueue
    (ser_in_push, ser_in_pop)   <- newQueue
    run port ser_in_push ser_out_pop
    return  (ser_in_pop, ser_out_push)
  where
  run port ser_in_push ser_out_pop = void $ forkIO $ bracket open hClose body
    where
    open = openSerial port (serBaud opts) 8 One NoParity NoFlowControl
    body h = do
      hSetBuffering h NoBuffering
      i <- asyncRunGW console "serial input" $ liftIO $ forever $ do
        c <- hGetChar h
        queuePush ser_in_push (B.pack [c2w c])

      o <- asyncRunGW console "serial output" $ liftIO $ forever $ do
        bs <- queuePop ser_out_pop
        B.hPutStr h bs
      wait i
      wait o
