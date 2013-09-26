-- See README for information on this module.

{-# LANGUAGE OverloadedStrings #-}

-- XXX Fix buffer sizes and where encryption/decryption happens.

module CommsecServer where

import qualified System.IO                  as I
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

import qualified Control.Concurrent.STM.TQueue as T
import qualified Control.Monad.STM as T

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

-- -- | Forever take an incoming bytestream, decodes the hxstream, decrypts it, and
-- -- sends the message on an outbound channel.
-- streamDecode ::
--      IO (Maybe B.ByteString)                   -- ^ bytestream source channel
--   -> (B.ByteString -> IO (Maybe B.ByteString)) -- ^ decryption
--   -> (B.ByteString -> IO ())                   -- ^ sink channel for decrypted
--                                                -- msgs
--   -> IO ()
-- streamDecode rx dec tx = loop H.emptyStreamState
--   where
--   loop hxSt = do
--     -- Yield if rx doesn't yield any bytes
--     mbs <- rx
--     -- Pattern match to force evaluation.
--     case mbs of
--       Nothing -> loop hxSt
--       Just bs -> withMsg bs
--     -- maybe (C.yield >> loop hxSt) withMsg =<< rx
--     where
--     withMsg bs = do
--       let (frames, hxSt') = H.decode bs hxSt
--       mDecFrames <- mapM dec frames
--       let decFrames = catMaybes mDecFrames
--       when (length decFrames /= length frames) $
--         putStrLn "Warning: couldn't decode some frames."
--       mapM_ tx decFrames
--       loop hxSt'

--   --     mapM_ (go newHxSt) frames
--   --     loop newHxSt
--   -- go hxSt bs = dec bs >>= maybe (loop hxSt) sendMsg
--     -- where
--     -- sendMsg msg =
--     --   if B.length msg > msgLen
--     --     then do
--     --       putStrLn "streamDecode: message greater than msgLen: frame dropped."
--     --       loop st
--     --   else tx msg >> loop st

-- --------------------------------------------------------------------------------
-- -- Encoding logic for outgoing streams.

-- -- | Take a bytestream source for Mavlink packets, frame them checking that
-- -- they're small enough, encrypt them, and then frame with hxstream.
-- streamEncode ::
--      IO (Maybe B.ByteString)                   -- ^ bytestream source channel
--   -> (B.ByteString -> IO (Maybe B.ByteString)) -- ^ encryption
--   -> (B.ByteString -> IO ())                   -- ^ bytestream sink channel
--   -> IO ()
-- streamEncode rx enc tx = loop L.emptyParseSt
--   where
--   loop :: L.ParseSt -> IO ()
--   loop ps =
--     -- Yield if rx doesn't yield any bytes.
--     maybe (C.yield >> loop ps) withMsg =<< rx
--     where

--     -- We got a bytestring; process it.
--     withMsg :: B.ByteString -> IO ()
--     withMsg msg = do
--       let (errs, packets, ps') = parseMavLinkStream ps msg
--       when (not $ null errs)
--         $ putStrLn ("Warning: mavlink parse errors: " ++ show errs)
--       mapM_ go packets >> loop ps'
-- --    withMsg msg = go (B.unpack msg) >> loop ps

--       where
--       go :: [Word8] -> IO ()
--       go = goEnc . B.pack

--       -- Try to encrypt and encode then transmit.
--       goEnc :: B.ByteString -> IO ()
--       goEnc mav = enc mav >>= maybe (return ()) (tx . encode)

--       encode = H.encode . B.unpack

-- --------------------------------------------------------------------------------
-- -- Set up TCP server for mavproxy

-- runTCP :: N.HostName
--        -> N.ServiceName
--        -> MVar
--        -> MVar
--        -> IO ()
-- runTCP host port fromMavMVar fromSerMVar =
--   S.serve (S.Host host) port $ \(mavSocket, _) -> do
--     putStrLn "Connected to mavproxy client..."

--     -- ctx <- initializeBase
--     -- _   <- C.forkIO $ streamDecode
--     --                     (M.tryTakeMVar fromSerMVar)
--     --                     (decrypt ctx)
--     --                     (S.send mavSocket)
--                         -- (\fr -> parseFrame fr)--  >>
--                                   -- S.send mavSocket fr)

--     _   <- C.forkOS $ forever
--              (do mbs <- M.tryTakeMVar fromSerMVar
--                  case mbs of
--                    Nothing -> return ()
--                    Just bs -> S.send mavSocket bs)
--              -- )
--              -- (
--              -- (S.send mavSocket)
--              -- (\fr -> parseFrame fr)
--              --   S.send mavSocket fr

--     forever $ do mrx <- S.recv mavSocket maxBytes
--                  case mrx of
--                    Nothing -> C.yield >> return ()
--                    Just rx -> nonBlockingPut fromMavMVar rx
--     -- streamEncode
--     --   (S.recv mavSocket maxBytes)
--     --   (encrypt ctx)
--     --   (M.putMVar fromMavMVar)

-- nonBlockingPut mvar v = do
--   mv <- M.tryTakeMVar mvar
--   case mv of
--     Nothing -> M.putMVar mvar v
--     Just v' -> M.putMVar mvar (v' `B.append` v)

-- --------------------------------------------------------------------------------
-- -- Set up serial

initializeUAV :: IO Context
initializeUAV = secPkgInit_HS uavID b2uSalt baseToUavKey u2bSalt uavToBaseKey

-- runSerial :: FilePath
--           -> MVar
--           -> MVar
--           -> IO ()
-- runSerial port fromMavMVar fromSerMVar = do

--     -- serialIn <- M.newEmptyMVar :: IO MVar

-- --  S.serve (S.Host "127.0.0.1") "6001" $ \(testSocket, _) -> do
--   P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
--     putStrLn "Connected to testing client..."
--     ctx <- initializeBase

--     P.flush s
--     -- _ <- C.forkIO $ forever $ do bs <- P.recv s maxBytes
--     --                              M.modifyMVar_ serialIn (\m -> m `B.append` bs)


--     -- 

--     -- _  <- C.forkOS $ forever $ void $ P.send s (B.pack [1,2,3,4,5])-- do mrx <- M.tr
--           -- yTakeMVar fromMavMVar
--                  -- case mrx of2
--                  --   Nothing -> C.yield >> return ()
--                  --   Just rx -> void $ P.send s rx


--     streamDecode
--             (do bs <- P.recv s maxBytes
--                 if B.null bs then return Nothing
--                   else return (Just bs))
--             (decrypt ctx)
--             (\bs -> do nonBlockingPut fromSerMVar bs
--                        void $ P.send s (B.pack [1,2,3,4,5])
--             )
--             -- parseFrame

--             -- nonBlockingPut fromSerMVar bs



--     -- _  <- C.forkIO $ streamDecode
--     --                    (do bs <- P.recv s maxBytes
--     --                        if B.null bs then return Nothing
--     --                           else return (Just bs)
--     --                    )
--     --                    (decrypt ctx)
--     --                    parseFrame

--     -- _  <- C.forkIO $ streamDecode
--     --        (do bs <- P.recv s maxBytes
--     --            if B.null bs then return Nothing
--     --              else return (Just bs))
--     --        (decrypt ctx)
--     --        parseFrame

--     -- streamDecode
--     --   (do bs <- P.recv s maxBytes
--     --       if B.null bs then return Nothing
--     --         else return (Just bs))
--     --   (decrypt ctx)
--     --   parseFrame

--                        -- (void . P.send s) -- Throw away unsent bytes.  Network
--                        --                   -- errors should be handled by the
--                        --                   -- protocol.


--     -- _  <- C.forkIO $ streamDecode
--     --                    (M.tryTakeMVar fromMavMVar)
--     --                    (decrypt ctx)
--     --                    (void . P.send s) -- Throw away unsent bytes.  Network
--     --                                      -- errors should be handled by the
--     --                                      -- protocol.
--     -- streamEncode
--     --   (fmap Just $ P.recv s maxBytes)
--     --   (encrypt ctx)
--     --   (M.putMVar fromSerMVar)

-- --------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--   -- XXX get opts properly
--   args <- getArgs
--   if length args /= 3
--     then putStrLn $ "Takes a host, port, and serial device as arguments "
--            ++ "(e.g., 127.0.0.1 6000 \"/dev/ttyUSB0\""
--      else run args

-- run :: [String] -> IO ()
-- run [host, port, serialPort] = do
--   _ <- printf ("Starting server on %s:%s and listening on serial device... %s"
--                ++ " at baud %s\n")
--          host port serialPort (show speed)

--   putStrLn "Enter to exit."

--   fromMavProxyMVar <- M.newEmptyMVar :: IO MVar
--   fromSerialMVar   <- M.newEmptyMVar :: IO MVar

--   -- Run the serial device server in a separate thread.
--   _ <- C.forkIO (runSerial serialPort fromMavProxyMVar fromSerialMVar)
--   -- Start TCP server to mavproxy
--   _ <- C.forkIO (runTCP host port fromMavProxyMVar fromSerialMVar)

--   quit
--   putStrLn "Exited."

--   where
--   quit = do
--     _ <- getLine
--     return ()
-- run _ = error "Bad arguments."

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
-- Testing

runSerialTest :: IO ()
runSerialTest =
  P.withSerial "/dev/ttyUSB0"
    P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
    putStrLn "connected"

    -- putStrLn "Connected to testing client..."
    ctx <- initializeBase

    rx <- T.newTQueueIO
    tx <- T.newTQueueIO

    P.flush s

    _ <- C.forkIO $ forever $ do
      bs <- P.recv s 1
      unless (B.null bs) (void $ T.atomically $ T.writeTQueue rx bs)

    _ <- C.forkIO $ forever $ do
      bs <- T.atomically $ T.readTQueue tx
      P.send s bs
--      P.setRTS s False
      -- let sender b = do P.send s (B.singleton b)
      --                   C.threadDelay 1000000
      -- mapM_ sender (B.unpack bs)

    _ <- C.forkIO $ forever $ do
        -- Only give data every second
      -- C.threadDelay 1000000

      let go b = do T.atomically $ T.writeTQueue tx (B.singleton b)
                    C.threadDelay 100000
      mapM_ go $ replicate 128 (0::Word8)



    -- let loop = forever $ do
          -- bs <- T.atomically $ T.readTQueue rx
          -- putStr $ "rx : " ++ show (B.length bs)
          -- T.atomically $ T.writeTQueue tx (A.pack "000")

    let loop hxSt = do
        bs <- T.atomically $ T.readTQueue rx
        let (frames, hxSt') = H.decode bs hxSt
        when (not $ null frames)
             (putStrLn $ "frame lens: "
                      ++ unwords (map (show . B.length) frames))
        -- let f fr = if B.length fr < 128 then putStrLn (show (B.unpack fr))
        --              else return ()
        -- mapM_ f frames
        mfrs <- mapM (decrypt ctx) frames
        mapM_ parseFrame (catMaybes mfrs)

        -- T.atomically $ T.writeTQueue tx (A.pack "000")

        loop hxSt'


    loop H.emptyStreamState

  -- -- let run hxSt = do
  -- --       bs <- R.atomicModifyIORef' rxRef (\bs' -> (B.empty, bs'))
  -- --       if B.null bs then run $! hxSt
  -- --         else do hxSt' <- loop bs hxSt
  -- --                 run $! hxSt'

  -- --       unless (B.null bs) $ (putStrLn . show . B.unpack) bs
  -- --       _ <- R.atomicModifyIORef' txRef (\_ -> (B.pack $ replicate 99 7, ()))
  -- --       return ()

  --   loop H.emptyStreamState


-- runSerialTest :: FilePath -> IO ()
-- runSerialTest port =
--   P.withSerial port P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do
--     P.flush s
--     putStrLn "Connected to testing client..."
--     ctx <- initializeBase
--     loop ctx H.emptyStreamState s
--     where
--     loop ctx hxSt s = do
--       rx <- P.recv s maxBytes
--       let (frames, hxSt') = H.decode rx hxSt
--       when (not $ null frames)
--            (putStrLn $ "frame lens: " ++ unwords (map (show . B.length) frames))
--       mfrs <- mapM (decrypt ctx) frames
--       -- when (length (catMaybes mfrs) /= length frames) $ do
--       --   let xs = zip mfrs frames
--       --   let f (Nothing, fr) = putStrLn $ "header: "
--       --                      ++ show (B.unpack fr)
--       --       f _             = return ()
--         -- mapM_ f xs
--       mapM_ parseFrame (catMaybes mfrs)

--       loop ctx hxSt' s

parseFrame fr = do
  putStrLn "parsing frame"
  let (errs, packets, _) = L.parseStream 128 L.emptyParseSt fr
  when (not $ null $ errs) $  putStrLn ("errs " ++ unlines errs)
  when (not $ null $ packets) $ putStrLn ("packets " ++ show packets)


runDev = do
  devH <- I.openFile "/dev/ttyUSB0" I.ReadMode
  rx <- B.hGetContents devH
  unless (B.null rx) (putStrLn $ show $ B.unpack rx)




runSerial2 :: IO ()
runSerial2 =
  P.withSerial "/dev/ttyUSB0"
    P.defaultSerialSettings { P.commSpeed = speed } $ \s -> do

    S.serve (S.Host "127.0.0.1") "6000" $ \(mavSocket, _) -> do
      putStrLn "Connected to mavproxy client..."

      putStrLn "connected"

      -- putStrLn "Connected to testing client..."
      ctx <- initializeBase

      rx <- T.newTQueueIO
      tx <- T.newTQueueIO

      -- Gratuitous flush.
      P.flush s

      -- Task to receive bytes from serial.
      _ <- C.forkIO $ forever $ do
        bs <- P.recv s 128
        unless (B.null bs) (void $ T.atomically $ T.writeTQueue rx bs)

      -- Task to send bytes to serial.
      _ <- C.forkIO $ rxLoop ctx s tx L.emptyParseSt

      -- Task to read bytes from mavproxy.
      _ <- C.forkIO $ forever $ do
        mbs <- S.recv mavSocket maxBytes
        -- Pattern match to force evaluation
        case mbs of
          Nothing -> return ()
          Just bs -> T.atomically $ T.writeTQueue tx bs

      txLoop ctx rx mavSocket H.emptyStreamState

--rxLoop :: L.ProcessSt -> B.ByteString -> IO ()
rxLoop ctx s tx processSt = do
  -- Read from mavproxy queue
  bs <- T.atomically $ T.readTQueue tx

  -- Parse mavlink up to 112 bytes.
  let (errs, packets, parseSt') = L.parseStream 112 processSt bs
  mapM_ putStrLn errs

  -- Pad the mavlink packets
  let paddedPacket bs =
        bs `B.append` (B.pack $ replicate (112 - B.length bs) 0)

  let paddedPackets = map paddedPacket packets

  -- Encrypt packets
  mencPackets <- mapM (secPkgEncInPlace_HS ctx) paddedPackets

  when (length (catMaybes mencPackets) /= length paddedPackets)
       (putStrLn "bad encryption")

  -- hxstream the packets
  let frames = map (H.encode . B.unpack) (catMaybes mencPackets)
  -- XXX
  putStrLn $ "To send frames: " ++ show (map B.unpack frames)
  mapM_ (P.send s) frames
  rxLoop ctx s tx parseSt'

txLoop ctx rx mavSocket hxSt = do
  bs <- T.atomically $ T.readTQueue rx
  let (frames, hxSt') = H.decode bs hxSt
  when (not $ null frames)
       (putStrLn $ "frame lens: "
                ++ unwords (map (show . B.length) frames))
  -- let f fr = if B.length fr < 128 then putStrLn (show (B.unpack fr))
  --              else return ()
  -- mapM_ f frames
  let frames' = filter (\bs' -> B.length bs' == 128) frames
  mfrs <- mapM (decrypt ctx) frames'
  let parsedFrames = catMaybes mfrs
  mapM_ parseFrame parsedFrames
  mapM_ (S.send mavSocket) parsedFrames

  -- T.atomically $ T.writeTQueue tx (A.pack "000")

  txLoop ctx rx mavSocket hxSt'
