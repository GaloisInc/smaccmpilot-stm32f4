{-# LANGUAGE OverloadedStrings #-}
{-

Read serial, perform encryption/decryption, and pass data between mavproxy.py
and SMACCMPilot.

------------  TCP  ------------------  Serial  ---------------
| mavproxy |  -->  | commsec-server |    -->   | SMACCMPilot |
------------  <--  ------------------    <--   ---------------

-}

module Main where

import           Data.Maybe
import           Text.Printf
import           System.Environment
import           System.Timeout
import           Control.Monad
import qualified Network.Simple.TCP         as S
import qualified Network.Socket             as N
import qualified System.Hardware.Serialport as P

import qualified Control.Concurrent         as C
import qualified Control.Concurrent.MVar    as M
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString            as Y

import           Data.Word
import qualified Data.Array.IO.Safe         as A
import qualified Data.Ix                    as I

import           Commsec

--------------------------------------------------------------------------------
-- State machine for collecting bytes and encrypting/decrypting them.

-- Number of bytes we're going to encrypt: 128 chunks minus 8 bytes for the
-- header (stationID | counter) and 8 bytes for the tag.
msgSize :: Int
msgSize = 128 - 8 - 8

uavID, baseID :: BaseId
uavID  = 0
baseID = 0

uavToBaseKey, baseToUavKey :: Y.ByteString
uavToBaseKey = Y.pack [0..15    :: Word8]
baseToUavKey = Y.pack [15,14..0 :: Word8]

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

type Ix = Int

-- Unboxed IOArray
type MsgBuf = A.IOUArray Ix Word8

initMsgBuf :: IO MsgBuf
initMsgBuf = A.newArray_ (0, msgSize-1)

initializeBase :: IO Context
initializeBase = secPkgInit_HS baseID u2bSalt uavToBaseKey b2uSalt baseToUavKey

bufReady :: MsgBuf -> Ix -> IO Bool
bufReady arr i = do
  rng <- A.getBounds arr
  return (I.inRange rng i)

packBuf :: MsgBuf -> IO Y.ByteString
packBuf buf = A.getElems buf >>= return . Y.pack

-- Encrypt the buffer, returning True if encryption was successful.
encryptBuf :: Context -> MsgBuf -> IO (Maybe Y.ByteString)
encryptBuf ctx buf = secPkgEncInPlace_HS ctx =<< packBuf buf

decryptBuf :: Context -> MsgBuf -> IO (Maybe Y.ByteString)
decryptBuf ctx buf = secPkgDec_HS ctx =<< packBuf buf

-- Fill up the buffer, returning values we couldn't fit in and the new index.
writesArray :: MsgBuf -> Ix -> Y.ByteString -> IO (Y.ByteString, Ix)
writesArray buf ix bs = do
  let wrds         = Y.unpack bs
  let remainingBuf = msgSize - ix
  let wrdsLen = Y.length bs
  if remainingBuf >= wrdsLen
    then inBuf wrds >> return (Y.empty, ix + msgSize)
    else do inBuf (take remainingBuf wrds)
            return ( Y.drop (wrdsLen - remainingBuf) bs, 0)
  where
  inBuf wrds = mapM_ (\(i, v) -> A.writeArray buf i v) (zip [ix..] wrds)

--------------------------------------------------------------------------------
-- Set up TCP server for mavproxy

runTCP :: N.HostName
       -> N.ServiceName
       -> M.MVar B.ByteString
       -> M.MVar B.ByteString
       -> IO ()
runTCP host port mavMVar serMVar =
  S.serve (S.Host host) port $ \(mavSocket, _) -> do
    putStrLn "Connected to mavproxy client..."

    ctx <- initializeBase
    buf <- initMsgBuf
    _ <- C.forkIO $ forever $ do
      -- See if there are contents in the serial MVar and pass them to mavproxy.
      -- Dont' block if none are available.
      sbs <- M.tryTakeMVar serMVar
      maybeUnit (S.send mavSocket) sbs

    loop mavSocket ctx buf 0

{-   --No crypto

      _ <- C.forkIO $ forever $ do

        -- See if there are contents in the serial MVar and pass them to
        -- mavproxy.  Dont' block if none are available.

        sbs <- M.tryTakeMVar serMVar
        maybeUnit (S.send mavSocket) sbs

      forever $ do
        -- Wait for 100 microseconds for input.
        mrx <- timeout theTimeout (S.recv mavSocket maxBytes)
        -- If there's anything, put them in the buffer.
        maybeUnit (M.putMVar mavMVar) (mm mrx)
 -}

    where
    loop mavSocket ctx buf ix = do
      ready <- bufReady buf ix
      when ready $ do
        pkg <- encryptBuf ctx buf
        when (isNothing pkg) (putStrLn "Warning! GCS encryption failed!")

        -- Send the whole thing to the serial thread.
        maybeUnit (M.putMVar mavMVar) pkg

        loop mavSocket ctx buf 0

      -- Wait for 100 microseconds for input.
      mrx <- timeout theTimeout (S.recv mavSocket maxBytes)
      -- If there's anything, put them in the buffer.
      (remB, ix') <- maybe (return (Y.empty, ix)) (writesArray buf ix) (mm mrx)
      unless (Y.null remB) (loop mavSocket ctx buf ix')

      loop mavSocket ctx buf ix'

--------------------------------------------------------------------------------
-- Set up serial

speed :: P.CommSpeed
speed = P.CS57600

runSerial :: FilePath -> M.MVar B.ByteString -> M.MVar B.ByteString -> IO ()
runSerial port mavMVar serMVar =
  P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do

    _ <- C.forkIO $ forever $ do
      -- See if there are contents in the MavLink MVar.  Don't block if none are
      -- available.
      mbs <- M.tryTakeMVar mavMVar
      maybeUnit (P.send s) mbs

    forever $ do
      -- Maybe read some bytes.  Timeout if nothing sent.
      mrx <- timeout theTimeout (P.recv s maxBytes)
      -- Send them to the TCP thread.
      maybeUnit (M.putMVar serMVar) mrx

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- XXX get opts properly
  args <- getArgs
  if length args /= 3
    then putStrLn $ "Takes a host port, and serial device as arguments "
           ++ "(e.g., 127.0.0.1 6000 \"/dev/ttyUSB0\""
     else run args

run :: [String] -> IO ()
run [host, port, serialPort] = do
  _ <- printf ("Starting server on %s:%s and listening on serial device... %s"
               ++ " at baud %s\n")
         host port serialPort (show speed)

  putStrLn "Enter to exit."

  fromMavProxyMVar <- M.newEmptyMVar :: IO (M.MVar B.ByteString)
  fromSerialMVar   <- M.newEmptyMVar :: IO (M.MVar B.ByteString)

  -- Run the serial device server in a separate thread.
  _ <- C.forkIO (runSerial serialPort fromMavProxyMVar fromSerialMVar)
  -- Start TCP server to mavproxy
  _ <- C.forkIO (runTCP host port fromMavProxyMVar fromSerialMVar)

  quit
  putStrLn "Exited."

  where
  quit = do
    _ <- getLine
    return ()
run _ = error "Bad arguments."

--------------------------------------------------------------------------------
-- Misc helpers

-- Number of bytes to receive at a time.
maxBytes :: Int
maxBytes = 1024

-- Microseconds to wait for a packet (note: this probably needs to be increased
-- on something other than localhost).
theTimeout :: Int
theTimeout = 100

maybeUnit :: (a -> IO b) -> Maybe a -> IO ()
maybeUnit f = maybe (return ()) (void . f)

--  Collapse Monad.
mm :: Monad m => m (m a) -> m a
mm = (id =<<)
