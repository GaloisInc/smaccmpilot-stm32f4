-- See README for information on this module.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CommsecServer
  ( commsecServer
  , Options(..)
  , defaultOpts
  ) where

import           System.IO
import           Data.Either
import           Data.Word
import           Data.Maybe
import           Control.Monad
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

import qualified SMACCMPilot.Shared            as S
import           Commsec

--------------------------------------------------------------------------------

data Options = Options
  { baseID   :: Word32  -- 0 - 15 uint32
  , baseKey  :: [Word8] -- 16 uint8s
  , baseSalt :: Word32  -- word32
  , uavKey   :: [Word8] -- 16 uint8s
  , uavSalt  :: Word32  -- word32
  , showErrs :: Bool    -- Show commsec errors
  } deriving (Show, Read, Eq)

defaultOpts :: Options
defaultOpts = Options
  { baseID   = 0
  , baseKey  = []
  , baseSalt = 0
  , uavKey   = []
  , uavSalt  = 0
  , showErrs = True
  }

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

type Error = String

writeError :: T.TQueue Error -> Error -> Commsec ()
writeError errQ = lift . T.atomically . T.writeTQueue errQ

-- | Decrypt and show errors.
decrypt :: T.TQueue Error -> Context -> Commsec ()
decrypt errQ ctx = do
  pkgs <- get
  eithers <- mapM (lift . secPkgDec_HS ctx) pkgs
  let (errs, frames) = partitionEithers eithers
  unless (null errs) $ do
         let showErrs = map ("Bad commsec decrypt from UAV: " ++)
                            (map show errs)
         mapM_ (writeError errQ) showErrs
  set frames

-- Pad out Mavlink packets.
paddedPacket :: B.ByteString -> B.ByteString
paddedPacket bs =
  bs `B.append` (B.pack $ replicate (fromInteger S.mavlinkSize - B.length bs) 0)

-- Filter out packets that aren't the right size and report errors.
filterCommsecLen :: T.TQueue Error -> Commsec ()
filterCommsecLen errQ = do
  bss <- get
  let frames = filter f bss
  when (length bss /= length frames)
       (writeError errQ "Bad commsec len from UAV")
  set frames
  where
  f bs = B.length bs == fromInteger S.commsecPkgSize

mavSize :: Word8
mavSize = fromInteger S.mavlinkSize

-- Helper to parse mavlink frames.
parseFrame :: T.TQueue Error -> Commsec ()
parseFrame errQ = do
  frames  <- get
  framess <- mapM parse frames
  set (concat framess)
  where
  parse :: B.ByteString -> Commsec [B.ByteString]
  parse frame = do
    let (errs, packets, _) = L.parseStream mavSize L.emptyParseSt frame
    unless (null errs) $ do
           let showErrs = map ("mavlink parse errors from UAV: " ++) errs
           mapM_ (writeError errQ) showErrs
    return packets
--------------------------------------------------------------------------------

commsecServer :: Options -> IO ()
commsecServer (Options ident baseKey b2uSalt uavKey u2bSalt showErrs) = do
  let baseToUavKey = B.pack baseKey
  let uavToBaseKey = B.pack uavKey
  let baseID       = fromIntegral ident

  -- Messages from the UAV
  rx <- T.newTQueueIO
  -- Messages from mavproxy
  tx <- T.newTQueueIO
  -- Collected error messages
  errQ <- T.newTQueueIO

  -- Handle all error reporting, if requested.
  _ <- C.forkIO $ forever $ do
    err <- T.atomically $ T.readTQueue errQ
    when showErrs $ do
      hPutStr stderr "Warning! commsec error:\n   "
      hPutStrLn stderr err

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
      _ <- C.forkIO $ forever $ do
        bs <- P.recv s 1024
        unless (B.null bs) (void $ T.atomically $ T.writeTQueue rx bs)

      -- Task to send bytes to serial.
      _ <- C.forkIO $ rxLoop errQ ctx s tx

      -- Task to read bytes from mavproxy.
      _ <- C.forkIO $ forever $ do
        mbs <- N.recv mavSocket 1024
        -- Pattern match to force evaluation
        case mbs of
          Nothing -> return ()
          Just bs -> T.atomically $ T.writeTQueue tx bs

      -- Task to decode messages from the UAV.
      txLoop errQ ctx rx mavSocket

--------------------------------------------------------------------------------

-- Decode messages from mavproxy and send them to the UAV.
rxLoop :: T.TQueue Error
       -> Context
       -> P.SerialPort
       -> T.TQueue B.ByteString
       -> IO ()
rxLoop errQ ctx s tx =
  fmap fst (run $ rxLoop' L.emptyParseSt)
  where
  rxLoop' processSt = do
    -- Read from mavproxy queue
    bs <- lift . T.atomically $ T.readTQueue tx

    -- Parse mavlink up to max-size bytes.
    let (errs, packets, parseSt') = L.parseStream mavSize processSt bs
    unless (null errs) (writeError errQ $ "mavlink parse errors from GCS: "
                                        ++ unlines errs)
    let paddedPackets = map paddedPacket packets
    -- Encrypt packets
    mencPackets <- lift $ mapM (secPkgEncInPlace_HS ctx) paddedPackets

    when (length (catMaybes mencPackets) /= length paddedPackets)
         (writeError errQ "bad encryption from GCS: ")
    -- hxstream the packets, all of which are for SMACCMPilot
    let frames = map (H.encode S.airDataTag) (catMaybes mencPackets)
    lift $ mapM_ (P.send s) frames
    set []
    rxLoop' parseSt'

--------------------------------------------------------------------------------

-- Decode messages from the UAV and send them to mavproxy.
txLoop :: T.TQueue Error
       -> Context
       -> T.TQueue B.ByteString
       -> N.Socket
       -> IO ()
txLoop errQ ctx rx mavSocket =
  fmap fst (run $ txLoop' H.emptyStreamState)
  where
  txLoop' hxSt = do
    -- Get messages from the serial device
    bs <- lift $ T.atomically $ T.readTQueue rx
    -- decode them
    let (tagframes, hxSt') = H.decode bs hxSt
    -- Get just the frames that are sent to the GCS.
    let frames = snd $ unzip $ filter ((== S.airDataTag) . fst) tagframes
    set frames
    -- Filter frames based on commsec length and report errors
    filterCommsecLen errQ
    -- decrypt them and report errors
    decrypt errQ ctx
    -- Parse mavlink and report errors
    parseFrame errQ
    lift . mapM_ (N.send mavSocket) =<< get
    set []
    txLoop' hxSt'

--------------------------------------------------------------------------------

