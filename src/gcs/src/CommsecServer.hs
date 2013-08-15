{-# LANGUAGE OverloadedStrings #-}
{-

Read serial, perform encryption/decryption, and pass data between mavproxy.py
and SMACCMPilot.

------------  TCP  ------------------  Serial  ---------------
| mavproxy |  -->  | commsec-server |    -->   | SMACCMPilot |
------------  <--  ------------------    <--   ---------------

-}

module Main where

import           System.Environment
import           System.Timeout
import           Control.Monad
import qualified Network.Simple.TCP         as S
import qualified Network.Socket             as N
import qualified System.Hardware.Serialport as P

import qualified Control.Concurrent         as C
import qualified Control.Concurrent.MVar    as M
import qualified Data.ByteString.Char8      as B

--------------------------------------------------------------------------------
-- Set up serial

speed :: P.CommSpeed
speed = P.CS57600

runSerial :: FilePath -> M.MVar B.ByteString -> M.MVar B.ByteString -> IO ()
runSerial port mavMVar serMVar = do
--  s <- P.openSerial port P.defaultSerialSettings { P.commSpeed = speed }
  P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s ->

    forever $ do
      -- Maybe read some bytes.
      mrx <- timeout 100 (P.recv s maxBytes)
      -- Send them to the TCP thread.  Don't block.
      maybeUnit (M.putMVar serMVar) mrx
      -- if (B.null sbs) then return ()
      --   else void (M.tryPutMVar serMVar sbs)
                -- XXX also shove them onto the terminal
                -- B.putStr sbs

      -- See if there are contents in the MavLink MVar.  Don't block.
      mbs <- M.tryTakeMVar mavMVar
      maybeUnit (P.send s) mbs
      -- maybeUnit (B.putStr) mbs

--------------------------------------------------------------------------------
-- Set up TCP server for mavproxy

runTCP :: N.HostName
       -> N.ServiceName
       -> M.MVar B.ByteString
       -> M.MVar B.ByteString
       -> IO ()
runTCP host port mavMVar serMVar =
  S.serve (S.Host host) port $ \(mavSocket, _) -> forever $ do
    -- Wait for 100 microseconds for input.
    mrx <- timeout 100 (S.recv mavSocket maxBytes)
    -- Send them to the serial thread.  Don't block.
    maybeUnit (M.putMVar mavMVar) (mm mrx)

    -- See if there are contents in the serial MVar and pass them to mavproxy.
    sbs <- M.tryTakeMVar serMVar
    maybeUnit (S.send mavSocket) sbs

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- XXX get opts properly
  [host, port, serialPort] <- getArgs
  putStrLn "Enter to exit."

  fromMavProxyMVar <- M.newEmptyMVar :: IO (M.MVar B.ByteString)
  fromSerialMVar   <- M.newEmptyMVar :: IO (M.MVar B.ByteString)

  -- Run the serial device server in a separate thread.
  _ <- C.forkIO (runSerial serialPort fromMavProxyMVar fromSerialMVar)
  -- Start TCP server to mavproxy
  _ <- C.forkIO (runTCP host port fromMavProxyMVar fromSerialMVar)

  quit
  putStrLn "Exited cleanly."

  where
  quit = do
    _ <- getLine
    return ()

--------------------------------------------------------------------------------
-- Helpers

-- Make this big enough so that it's unlikely the buffer will fill and we'll
-- block.
maxBytes :: Int
maxBytes = 1

maybeUnit :: (a -> IO b) -> Maybe a -> IO ()
maybeUnit f = maybe (return ()) (void . f)

--  Collapse Monad.
mm :: Monad m => m (m a) -> m a
mm m = do x <- m
          y <- x
          return y
