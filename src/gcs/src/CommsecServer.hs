-- See README for information on this module.

{-# LANGUAGE OverloadedStrings #-}

-- XXX Fix buffer sizes and where encryption/decryption happens.

module Main where

import           Text.Printf
import           System.Environment
import           Control.Monad
import qualified Network.Socket             as N
import qualified Network.Simple.TCP         as S
import qualified System.Hardware.Serialport as P
import qualified Control.Concurrent         as C
import qualified Control.Concurrent.MVar    as M
import qualified Data.ByteString            as B
import           Data.Word
import qualified Data.HXStream              as H
import           Commsec
import qualified Mavlink.Parser             as L

-- XXXX Testing
import Data.Maybe
--------------------------------------------------------------------------------
-- State machine for collecting bytes and encrypting/decrypting them.

-- Number of bytes in a message: 128 chunks minus 8 bytes for the
-- header (stationID | counter) and 8 bytes for the tag.
pkgLen, msgLen :: Int
pkgLen = 128
msgLen = pkgLen - 8 - 8

uavID, baseID :: BaseId
uavID  = 5
baseID = 7

uavToBaseKey, baseToUavKey :: B.ByteString
uavToBaseKey = B.pack [0..15    :: Word8]
baseToUavKey = B.pack [15,14..0 :: Word8]

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

initializeBase :: IO Context
initializeBase = secPkgInit_HS baseID u2bSalt uavToBaseKey b2uSalt baseToUavKey

type MVar = M.MVar B.ByteString

--------------------------------------------------------------------------------
-- Frame Mavlink packets

-- | Parse Mavlink packets.  Returns the parsed packets and the rest of the
-- bytestream.  Packets that are too big or malformed are dropped.
parseMavLinkStream :: L.ParseSt -> B.ByteString -> L.ProcessSt
parseMavLinkStream = L.parseStream m
  where
  -- mkPkt = padMsg . B.pack
  m = if fromIntegral (maxBound :: Word8) < pkgLen
        then error "Pkg len must be less than 255."
        else fromIntegral pkgLen

-- Testing
-- (Just (B.take 5 bs), B.drop 5 bs)

-- | Pad messages to make them msgLen bytes.  Assume messages are no greater
-- than msgLen.
padMsg :: B.ByteString -> B.ByteString
padMsg bs =
  let len = B.length bs in
  if len > msgLen then error "padMsg requires messages <= msgLen."
  else B.append bs $ B.replicate (msgLen - len) 0

--------------------------------------------------------------------------------
-- Encrypt/decrypt AES-GCM packets

-- | Decrypt function.
decrypt :: Context -> B.ByteString -> IO (Maybe B.ByteString)
decrypt ctx pkg = do
  msg <- secPkgDec_HS ctx pkg
  case msg of
    Left err  -> (putStrLn $ "Warning: " ++ show err) >> return Nothing
    Right res -> return (Just res)

-- | Encrypt function.
encrypt :: Context -> B.ByteString -> IO (Maybe B.ByteString)
encrypt ctx bs = do
  pkg <- secPkgEncInPlace_HS ctx bs
  maybe (putStrLn "Warning: encryption failed!" >> return Nothing)
        (return . Just)
        pkg

--------------------------------------------------------------------------------
-- Decoding logic for incoming streams.

-- | Forever take an incoming bytestream, decodes the hxstream, decrypts it, and
-- sends the message on an outbound channel.
streamDecode ::
     IO (Maybe B.ByteString)                   -- ^ bytestream source channel
  -> (B.ByteString -> IO (Maybe B.ByteString)) -- ^ decryption
  -> (B.ByteString -> IO ())                   -- ^ sink channel for decrypted
                                               -- msgs
  -> IO ()
streamDecode rx dec tx = loop H.emptyStreamState
  where
  loop st =
    -- Yield if rx doesn't yield any bytes
    maybe (C.yield >> loop st) withMsg =<< rx
    where
    withMsg bs = do
      let (frames, newSt) = H.decode bs st
      mapM_ (go newSt) frames
      loop newSt
  go st bs = dec bs >>= maybe (loop st) sendMsg
    where
    sendMsg msg =
      if B.length msg > msgLen
        then do
          putStrLn "streamDecode: message greater than msgLen: frame dropped."
          loop st
      else tx msg >> loop st

--------------------------------------------------------------------------------
-- Encoding logic for outgoing streams.

-- | Take a bytestream source for Mavlink packets, frame them checking that
-- they're small enough, encrypt them, and then frame with hxstream.
streamEncode ::
     IO (Maybe B.ByteString)                   -- ^ bytestream source channel
  -> (B.ByteString -> IO (Maybe B.ByteString)) -- ^ encryption
  -> (B.ByteString -> IO ())                   -- ^ bytestream sink channel
  -> IO ()
streamEncode rx enc tx = loop L.emptyParseSt
  where
  loop :: L.ParseSt -> IO ()
  loop ps =
    -- Yield if rx doesn't yield any bytes.
    maybe (C.yield >> loop ps) withMsg =<< rx
    where

    -- We got a bytestring; process it.
    withMsg :: B.ByteString -> IO ()
    withMsg msg = do
      let (errs, packets, ps') = parseMavLinkStream ps msg
      when (not $ null errs)
        $ putStrLn ("Warning: mavlink parse errors: " ++ show errs)
      mapM_ go packets >> loop ps'
--    withMsg msg = go (B.unpack msg) >> loop ps

      where
      go :: [Word8] -> IO ()
      go = goEnc . B.pack

      -- Try to encrypt and encode then transmit.
      goEnc :: B.ByteString -> IO ()
      goEnc mav = enc mav >>= maybe (return ()) (tx . encode)

      encode = H.encode . B.unpack

--------------------------------------------------------------------------------
-- Set up TCP server for mavproxy

runTCP :: N.HostName
       -> N.ServiceName
       -> MVar
       -> MVar
       -> IO ()
runTCP host port fromMavMVar fromSerMVar =
  S.serve (S.Host host) port $ \(mavSocket, _) -> do
    putStrLn "Connected to mavproxy client..."
    ctx <- initializeBase
    _   <- C.forkIO $ streamDecode
                        (M.tryTakeMVar fromSerMVar)
                        (decrypt ctx)
                        (S.send mavSocket)
    streamEncode
      (S.recv mavSocket maxBytes)
      (encrypt ctx)
      (M.putMVar fromMavMVar)

--------------------------------------------------------------------------------
-- Set up serial

initializeUAV :: IO Context
initializeUAV = secPkgInit_HS uavID b2uSalt baseToUavKey u2bSalt uavToBaseKey

runSerial :: FilePath
          -> MVar
          -> MVar
          -> IO ()
runSerial port fromMavMVar fromSerMVar =
  S.serve (S.Host "127.0.0.1") "6001" $ \(testSocket, _) -> do
--  P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
    putStrLn "Connected to testing client..."
    ctx <- initializeUAV
    _  <- C.forkIO $ streamDecode
                       (M.tryTakeMVar fromMavMVar)
                       (decrypt ctx)
                       (S.send testSocket)
    streamEncode
      (S.recv testSocket maxBytes)
      (encrypt ctx)
      (M.putMVar fromSerMVar)


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

  fromMavProxyMVar <- M.newEmptyMVar :: IO MVar
  fromSerialMVar   <- M.newEmptyMVar :: IO MVar

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

-- Serial speed
speed :: P.CommSpeed
speed = P.CS57600

-- Number of bytes to receive at a time.
maxBytes :: Int
maxBytes = 1024

-- Microseconds to wait for a packet (note: this probably needs to be increased
-- on something other than localhost).
theTimeout :: Int
theTimeout = 100

maybeUnit :: (a -> IO b) -> Maybe a -> IO ()
maybeUnit f = maybe (return ()) (void . f)

--------------------------------------------------------------------------------

-- Testing: just parse the mavlink messages from serial.
-- runSerialTest "/dev/ttyUSB0"
-- runSerialTest :: FilePath -> IO ()
-- runSerialTest port =
--   P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
--     putStrLn "Connected to testing client..."
--     ctx <- initializeBase
--     loop ctx H.emptyStreamState s
--     where
--     loop ctx hxSt s = do
--       rx <- P.recv s maxBytes
--       -- putStrLn $ "got " ++ show (B.length rx) ++ " bytes"
--       -- when (not $ B.null rx) (putStrLn $ "   bytes: "
--       --                            ++ show (B.unpack $ B.take 12 rx))
--       let (frames, hxSt') = H.decode rx hxSt
--       -- mfrs <- mapM (decrypt ctx) frames
--       -- when (length (catMaybes mfrs) /= length frames) (putStrLn "bad decrypt")
--       mapM_ parseFrame frames

--       loop ctx hxSt' s

--     parseFrame fr = do
--       let (errs, packets, _) = L.parseStream 128 L.emptyParseSt fr
--       when (not $ null $ errs) $  putStrLn ("errs " ++ unlines errs)
--       when (not $ null $ packets) $ putStrLn ("packets " ++ show packets)

runSerialTest :: FilePath -> IO ()
runSerialTest port =
  P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
    putStrLn "Connected to testing client..."
    ctx <- initializeBase
    loop ctx H.emptyStreamState s
    where
    loop ctx hxSt s = do
      rx <- P.recv s maxBytes
      -- putStrLn $ "got " ++ show (B.length rx) ++ " bytes"
      -- when (not $ B.null rx) (putStrLn $ "   bytes: "
      --                            ++ show (B.unpack $ B.take 12 rx))
      let (frames, hxSt') = H.decode rx hxSt
      when (not $ null frames) (putStrLn $ "frame lens: " ++ unwords (map (show . B.length) frames))
      mfrs <- mapM (decrypt ctx) frames
      when (length (catMaybes mfrs) /= length frames) $ do
        let xs = zip mfrs frames
        let f (Nothing, fr) = putStrLn $ "header: " ++ show (B.unpack $ B.take 8 fr)
            f _             = return ()
        mapM_ f xs
      mapM_ parseFrame (catMaybes mfrs)

      loop ctx hxSt' s

    parseFrame fr = do
      putStrLn $ "packet: " ++ show (B.unpack fr) -- $ B.take 8 fr)
      let (errs, packets, _) = L.parseStream 128 L.emptyParseSt fr
      when (not $ null $ errs) $  putStrLn ("errs " ++ unlines errs)
      when (not $ null $ packets) $ putStrLn ("packets " ++ show packets)

-- -- Testing: just parse the mavlink packets: no hx, no crypto.
-- runSerialTest :: FilePath -> IO ()
-- runSerialTest port =
--   P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
--     putStrLn "Connected to testing client..."
--     loop L.emptyParseSt s
--     where
--     loop ps s = do
--       rx <- P.recv s maxBytes
--       let (errs, packets, ps') = L.parseStream 128 ps rx
--       when (not $ null $ errs) $  putStrLn ("errs " ++ unlines errs)
--       when (not $ null $ packets) $ putStrLn ("packets " ++ show packets)
--       loop ps' s
