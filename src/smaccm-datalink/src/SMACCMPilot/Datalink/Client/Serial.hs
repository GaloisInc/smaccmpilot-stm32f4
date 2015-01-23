{-# LANGUAGE ScopedTypeVariables #-}
module SMACCMPilot.Datalink.Client.Serial
  ( serialServer
  ) where

import           Data.Word
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
             -> Pushable Word8
             -> Poppable ByteString
             -> IO ()
serialServer opts console fromser toser = case serPort opts of
  Nothing -> hPutStrLn stderr "error: must provide a serial port" >> exitFailure
  Just port -> run port
  where
  run port = void $ forkIO $ bracket (open port) hClose body
  open port = openSerial port (serBaud opts) 8 One NoParity NoFlowControl
  body h = do
    hSetBuffering h NoBuffering
    i <- asyncRunGW console "serial input" $ liftIO $ forever $ do
      c <- hGetChar h
      queuePush fromser (c2w c)

    _ <- asyncRunGW console "serial output" $ liftIO $ forever $ do
      bs <- queuePop toser
      B.hPutStr h bs
    wait i
