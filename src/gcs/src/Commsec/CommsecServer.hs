-- See README for information on this module.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Commsec.CommsecServer
  ( commsecServer
  ) where

import           System.IO
import           Data.Either
import           Data.Word
import           Data.Maybe
import           Data.IORef
import           Control.Monad
import           Text.Printf                   as PF
import qualified MonadLib                      as M
import qualified Control.Concurrent            as C
import qualified Control.Concurrent.STM.TQueue as T
import qualified Control.Monad.STM             as T

import qualified Network.Simple.TCP            as N
import           Network()
import qualified System.Hardware.Serialport    as P
import qualified Data.ByteString               as B
import qualified Data.HXStream                 as H
import qualified Mavlink.Parser                as L
import qualified Mavlink.MessageName           as ML

import qualified SMACCMPilot.Communications    as C
import           Commsec.Commsec
import           Commsec.CommsecOpts

--------------------------------------------------------------------------------
-- Monad definition.  The state is the good frames.

newtype Commsec a = Commsec (M.StateT [B.ByteString] IO a) deriving Monad

lift :: IO a -> Commsec a
lift = Commsec . M.lift

set :: [B.ByteString] -> Commsec ()
set = Commsec . M.set

get :: Commsec [B.ByteString]
get = Commsec M.get

run :: Commsec a -> IO (a, [B.ByteString])
run (Commsec m) = M.runStateT [] m

--------------------------------------------------------------------------------
-- Helper functions.

data Console = Error String | Log String

writeError :: T.TQueue Console ->  String -> Commsec ()
writeError conQ  = lift . T.atomically . T.writeTQueue conQ . Error
writeLog :: T.TQueue Console ->  String -> Commsec ()
writeLog  conQ msg = lift (writeLogIO conQ msg)

writeLogIO :: T.TQueue Console ->  String ->  IO ()
writeLogIO conQ  = T.atomically . T.writeTQueue conQ . Log

-- | Decrypt and show errors.
decrypt :: T.TQueue Console -> Context -> Commsec ()
decrypt conQ ctx = do
  pkgs <- get
  eithers <- mapM (lift . secPkgDec_HS ctx) pkgs
  let (errs, frames) = partitionEithers eithers
  unless (null errs) $ do
         let showErrs = map ("Bad commsec decrypt from UAV: " ++)
                            (map show errs)
         mapM_ (writeError conQ) showErrs
  set frames

-- Pad out Mavlink packets.
paddedPacket :: B.ByteString -> B.ByteString
paddedPacket bs =
  bs `B.append` (B.pack $ replicate (fromInteger C.mavlinkSize - B.length bs) 0)

-- Filter out packets that aren't the right size and report errors.
filterCommsecLen :: T.TQueue Console -> Commsec ()
filterCommsecLen conQ = do
  bss <- get
  let frames = filter f bss
  when (length bss /= length frames)
       (writeError conQ "Bad commsec len from UAV")
  set frames
  where
  f bs = B.length bs == fromInteger C.commsecPkgSize

mavSize :: Word8
mavSize = fromInteger C.mavlinkSize

showMLFrame :: B.ByteString -> String
showMLFrame bs = concat $ markpacketid $ map (PF.printf "0x%0.2x ") (B.unpack bs)
  where
  markpacketid :: [String] -> [String]
  markpacketid ps | length ps > 5 = (take 5 ps) ++ [ML.messagename (read (ps !! 5))] ++ (drop 5 ps)
                  | otherwise = ps

-- Helper to parse mavlink frames.
parseFrame :: T.TQueue Console -> Commsec ()
parseFrame conQ = do
  frames  <- get
  framess <- mapM parse frames
  set (concat framess)
  when ((length framess) > 0) $ log $ show (map showMLFrame (concat framess))
  where
  log msg = writeLog conQ ("FROMVEH MLPARSE " ++ msg)
  parse :: B.ByteString -> Commsec [B.ByteString]
  parse frame = do
    let (errs, packets, _) = L.parseStream mavSize L.emptyParseSt frame
    unless (null errs) $ do
           let showErrs = map ("mavlink parse errors from UAV: " ++) errs
           mapM_ (writeError conQ) showErrs
    return packets
--------------------------------------------------------------------------------

commsecServer :: Options -> IO ()
commsecServer (Options ident baseKey b2uSalt uavKey u2bSalt loglevel) = do
  let baseToUavKey = B.pack baseKey
  let uavToBaseKey = B.pack uavKey
  let baseID       = fromIntegral ident

  -- Messages from the UAV
  rx <- T.newTQueueIO
  -- Messages from mavproxy
  tx <- T.newTQueueIO
  -- Console messages
  conQ <- T.newTQueueIO

  -- Handle all error reporting, if requested.
  _ <- C.forkIO $ forever $ do
    console <- T.atomically $ T.readTQueue conQ
    case console of
        Error msg -> when (loglevel > 0) $ do
          hPutStrLn stderr ("ERR: " ++ msg)
        Log msg -> when (loglevel > 1) $ do
          hPutStrLn stderr ("LOG: " ++ msg)

  -- Initialize commsec context
  ctx <- secPkgInit_HS baseID u2bSalt uavToBaseKey b2uSalt baseToUavKey

  P.withSerial "/dev/ttyUSB0"
    P.defaultSerialSettings { P.commSpeed = P.CS57600 } $ \s -> do

    putStrLn "Connected to serial client..."

    N.serve (N.Host "127.0.0.1") "6000" $ \(mavSocket, _) -> do
      putStrLn "Connected to mavproxy client..."

      -- Gratuitous flush.
      P.flush s

      -- Task to receive bytes from serial.
      _ <- C.forkIO $ do
        let log msg = writeLogIO conQ ("FROMVEH " ++ msg)
        logByteCount <- mkLogByteCount log
        forever $ do
          bs <- P.recv s 1024
          unless (B.null bs) $ do
            logByteCount bs
            announceFBOs log bs
            (void $ T.atomically $ T.writeTQueue rx bs)

      -- Task to send bytes to serial.
      _ <- C.forkIO $ rxLoop conQ ctx s tx

      -- Task to read bytes from mavproxy.
      _ <- C.forkIO $ forever $ do
        mbs <- N.recv mavSocket 1024
        -- Pattern match to force evaluation
        case mbs of
          Nothing -> return ()
          Just bs -> T.atomically $ T.writeTQueue tx bs

      -- Task to decode messages from the UAV.
      txLoop conQ ctx rx mavSocket
  where
  mkLogByteCount log = do
    counter <- newIORef 0
    return $ \bs -> do
      c <- readIORef counter
      let c' = c + (B.length bs)
      writeIORef counter c'
      log ((show c') ++ " bytes")
  announceFBOs log bs = do
    mapM_ (const (log "FBO")) fbos
    where
    fbos = filter (== H.fbo) (B.unpack bs)
--------------------------------------------------------------------------------

-- Decode messages from mavproxy and send them to the UAV.
rxLoop :: T.TQueue Console
       -> Context
       -> P.SerialPort
       -> T.TQueue B.ByteString
       -> IO ()
rxLoop conQ ctx s tx =
  fmap fst (run (rxLoop' L.emptyParseSt))
  where
  log msg = writeLog conQ ("TOVEH " ++ msg)
  rxLoop' processSt = do
    -- Read from mavproxy queue
    bs <- lift . T.atomically $ T.readTQueue tx

    -- Parse mavlink up to max-size bytes.
    let (errs, packets, parseSt') = L.parseStream mavSize processSt bs
    unless (null errs) (writeError conQ $ "mavlink parse errors from GCS: "
                                        ++ unlines errs)
    when (length packets > 0) $ log ("MAVPARSE " ++ (show (map showMLFrame packets)))
    let paddedPackets = map paddedPacket packets
    -- Encrypt packets
    mencPackets <- lift $ mapM (secPkgEncInPlace_HS ctx) paddedPackets

    when (length (catMaybes mencPackets) /= length paddedPackets)
         (writeError conQ "bad encryption from GCS: ")
    -- hxstream the packets, all of which are for SMACCMPilot
    let frames = map (H.encode C.airDataTag) (catMaybes mencPackets)
    lift $ mapM_ (P.send s) frames
    set []
    rxLoop' parseSt'

--------------------------------------------------------------------------------

-- Decode messages from the UAV and send them to mavproxy.
txLoop :: T.TQueue Console
       -> Context
       -> T.TQueue B.ByteString
       -> N.Socket
       -> IO ()
txLoop conQ ctx rx mavSocket =
  fmap fst (run $ txLoop' H.emptyStreamState)
  where
  txLoop' :: H.StreamState -> Commsec ()
  txLoop' hxSt = do
    -- Get messages from the serial device
    bs <- lift $ T.atomically $ T.readTQueue rx
    -- decode them
    let (tagframes, hxSt') = H.decode bs hxSt
    -- Get just the frames that are sent to the GCS.
    let frames = snd $ unzip $ filter ((== C.airDataTag) . fst) tagframes
    set frames
    -- Filter frames based on commsec length and report errors
    filterCommsecLen conQ
    -- decrypt them and report errors
    decrypt conQ ctx
    -- Parse mavlink and report errors
    parseFrame conQ
    lift . mapM_ (N.send mavSocket) =<< get
    set []
    txLoop' hxSt'

--------------------------------------------------------------------------------
