{-# LANGUAGE OverloadedStrings #-}
{-

Read serial, perform encryption/decryption, and pass data between mavproxy.py
and SMACCMPilot.

Here's the architecture:

------------  TCP  ------------------  Serial  ---------------
| mavproxy |  -->  | commsec-server |    -->   | SMACCMPilot |
------------  <--  ------------------    <--   ---------------

Inside the commsec-server (this module) is the following:

        ---------------------------------------
        |               fromSerMVar            |
        |               ---------->            |
        | SerialHandler             TCPHandler |
        |               fromTCPMVar            |
        |               <----------            |
        ---------------------------------------

The two handlers talk back and forth via MVars.

Inside each handler are two data buffers.  For example, in the TCPHandler, we
have
                           ------------------------
  to mavproxy (plaintext)  |                      | fromSerMVar (encrypted)
 <-------------------------|-- buffer (pkgSize) <-|------------------------
 from mavproxy (plaintext) |                      | fromTCPMVar (plaintext)
 --------------------------|-> buffer (msgSize) --|----------------------->
                           |                      |
                           ------------------------

and in the SerialHandler we have

                              ------------------------
  to SMACCMPilot (encrypt)    |                      | fromTCPMVar (plaintext)
 <----------------------------|-- buffer (msgSize) <-|------------------------
 from SMACCMPilot (encrypted) |                      | fromSerMVar (encrypted)
 -----------------------------|-> buffer (pkgSize) --|----------------------->
                              |                      |
                              ------------------------

The buffers accumulate data until we have a package big enough to encrypt and
send over.  Note that buffers from the MVars buffer an entire package (including
the header) and not just the message itself.

------------------
- Message format -
------------------

Encrypted messages sent to the autopilot or to the GCS from the CommsecServer
are fixed-width byte-arrays set by the pkgSize constant.  It containts the
header, consisting of an identifier (8 bytes) and a counter (8 bytes), and the
encrypted message:

  --------------------
  | id | cnter | msg |
  --------------------

These messages are framed by the hxstream protocol to encode the beginning and
end of the payload to enable resynchronization in case of dropped bytes:

  -------------------------------------------
  | 0x7e | encoded(id | cnter | msg) | 0x7e |
  -------------------------------------------

Note that the hxstream protocol does a slight encoding of the payload, but there
is no CRC, etc.

XXX We probabilisticaly assume the data array is large enough to hold.

-}

-- XXX Fix buffer sizes and where encryption/decryption happens.

module Main where

import           Data.Maybe
import           Text.Printf
import           System.Environment
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
pkgSize, msgSize :: Int
pkgSize = 128
msgSize = pkgSize - 8 - 8

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

initMsgBuf :: Int -> IO MsgBuf
initMsgBuf sz = A.newArray_ (0, sz-1)

initializeBase :: IO Context
initializeBase = secPkgInit_HS baseID u2bSalt uavToBaseKey b2uSalt baseToUavKey

bufReady :: MsgBuf -> Ix -> IO Bool
bufReady arr i = do
  rng <- A.getBounds arr
  return $ not (I.inRange rng i)

extractBuf :: MsgBuf -> IO Y.ByteString
extractBuf buf = A.getElems buf >>= return . Y.pack

-- Fill up the buffer, returning values we couldn't fit in and the new index.
writesArray :: MsgBuf -> Ix -> Y.ByteString -> IO (Y.ByteString, Ix)
writesArray buf ix bs = do
  top             <- return . (+1) . snd =<< A.getBounds buf
  let wrds         = Y.unpack bs
  let wrdsLen      = length wrds
  let remainingBuf = top - ix
--  if remainingBuf >= wrdsLen
--    then inBuf wrds >> return (Y.empty, ix + wrdsLen)
  inBuf (take remainingBuf wrds)
  return ( Y.drop remainingBuf bs, min top (ix + wrdsLen))
  where
  inBuf wrds = mapM_ (\(i, v) -> A.writeArray buf i v) (zip [ix, ix+1 ..] wrds)

--------------------------------------------------------------------------------

-- Buffered handler.  The first action is the action to perform when the buffer
-- is full and the second is the action to perform to fill the buffer.
bufferedExec :: Int
             -> (Y.ByteString -> IO ())
             -> IO (Maybe Y.ByteString)
             -> IO ()
bufferedExec sz fullAction fillAction = do
  buf <- initMsgBuf sz
  loop buf Y.empty 0
  where
  loop buf bytes ix = do
    ready <- bufReady buf ix
    if ready
      then do
        pkg <- extractBuf buf
        fullAction pkg
        -- Now put any leftover bytes into our buffer.
        (remB, ix') <- writesArray buf 0 bytes
        loop buf remB ix'
      else do
        bs <- fillAction
        -- If there's anything, put them in the buffer.
        (remB, ix') <- maybe (return (bytes, ix))
                             (writesArray buf ix)
                             bs
        loop buf remB ix'

--------------------------------------------------------------------------------

-- Helper decrypt function.
decFrom :: Context -> (Y.ByteString -> IO ()) -> Y.ByteString -> IO ()
decFrom ctx action pkg = do
  msg <- secPkgDec_HS ctx pkg
  when (isNothing msg) (putStrLn "Decryption failed!")
  maybeUnit action msg

-- Helper encrypt function.
encTo :: Context -> (Y.ByteString -> IO ()) -> Y.ByteString -> IO ()
encTo ctx action bs = do
  pkg <- secPkgEncInPlace_HS ctx bs
  when (isNothing pkg) (putStrLn "Warning! GCS encryption failed!")
  maybeUnit action pkg

--------------------------------------------------------------------------------
-- Set up TCP server for mavproxy

runTCP :: N.HostName
       -> N.ServiceName
       -> M.MVar B.ByteString
       -> M.MVar B.ByteString
       -> IO ()
runTCP host port fromMavMVar fromSerMVar =
  S.serve (S.Host host) port $ \(mavSocket, _) -> do
    putStrLn "Connected to mavproxy client..."

    ctx <- initializeBase
    -- To TCP socket:
    -- When the buffer is full, send to the socket.  Fill with bytes from the
    -- serial MVar.
    _   <- C.forkIO $ bufferedExec
                        pkgSize
                        (decFrom ctx (S.send mavSocket))
                        (M.tryTakeMVar fromSerMVar)

    -- To Serial:
    -- When the buffer is full, put bytes into the outbound MVar.  Fill the
    -- buffer by receiving bytes from the socket.
    bufferedExec
      msgSize
      (encTo ctx (M.putMVar fromMavMVar))
      (S.recv mavSocket maxBytes)

--------------------------------------------------------------------------------
-- Set up serial

speed :: P.CommSpeed
speed = P.CS57600

{-
runSerial :: FilePath -> M.MVar B.ByteString -> M.MVar B.ByteString -> IO ()
runSerial port fromMavMVar fromSerMVar =
  P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do

    _ <- C.forkIO $ forever $ do
      -- See if there are contents in the MavLink MVar.  Don't block if none are
      -- available.
      mbs <- M.tryTakeMVar fromMavMVar
      maybeUnit (P.send s) mbs

    forever $ do
      -- Maybe read some bytes.  Timeout if nothing sent.
      mrx <- P.recv s maxBytes
      -- Send them to the TCP thread.
      maybeUnit (M.putMVar fromSerMVar) mrx
-}

initializeUAV :: IO Context
initializeUAV = secPkgInit_HS uavID b2uSalt baseToUavKey u2bSalt uavToBaseKey

runSerial :: FilePath -> M.MVar B.ByteString -> M.MVar B.ByteString -> IO ()
runSerial port fromMavMVar fromSerMVar =
  S.serve (S.Host "127.0.0.1") "6001" $ \(testSocket, _) -> do
    putStrLn "Connected to testing client..."

    ctx <- initializeUAV

    -- To serial socket:
    -- When the buffer is full, send to the socket.  Fill with bytes from the
    -- fromMavMVar.
    _  <- C.forkIO $ bufferedExec
                       pkgSize
                       (decFrom ctx (S.send testSocket))
                       (M.tryTakeMVar fromMavMVar)

    -- To TCP:
    -- When the buffer is full, put bytes into the outbound MVar.  Fill the
    -- buffer by receiving bytes from the socket.
    bufferedExec
      msgSize
      (encTo ctx (M.putMVar fromSerMVar))
      (S.recv testSocket maxBytes)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- XXX get opts properly
  args <- getArgs
  if length args /= 3
    then putStrLn $ "Takes a host, port, and serial device as arguments "
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
