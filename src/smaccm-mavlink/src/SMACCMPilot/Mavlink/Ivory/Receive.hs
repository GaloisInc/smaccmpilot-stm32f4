{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Mavlink.Receive where

import Data.Maybe
import Ivory.Language
import Ivory.Stdlib
import SMACCMPilot.Mavlink.CRC
import SMACCMPilot.Mavlink.Messages (messageLensCRCs)

[ivory|
struct mavlink_receive_state
  { paylen   :: Stored Uint8
  ; seqnum   :: Stored Uint8
  ; sysid    :: Stored Uint8
  ; compid   :: Stored Uint8
  ; msgid    :: Stored Uint8
  ; payload  :: Array 256 (Stored Uint8)
  ; crc      :: Stored CRC
  }
|]

mavlinkReceiveStateModule :: Module
mavlinkReceiveStateModule = package "mavlink_receive_state" $ do
  defStruct (Proxy :: Proxy "mavlink_receive_state")
  defConstMemArea msgLengths
  defConstMemArea msgCRCExtras

mavlink_STX :: Uint8
mavlink_STX = 254

msgLengths, msgCRCExtras :: ConstMemArea (Array 256 (Stored Uint8))
(msgLengths, msgCRCExtras) = (makeTable "message_length" lengths, makeTable "message_crc_extra" crcs)
  where
  makeTable nm xs = constArea nm $ iarray $ map (ival . fromIntegral) xs
  (lengths, crcs) = unzip [ fromMaybe (0, 0) $ lookup i messageLensCRCs | i <- [0..255] ]

mavlinkReceiver :: (forall s1. Ivory (AllocEffects s1) ()) -> (forall s2 s3. ConstRef s2 (Struct "mavlink_receive_state") -> Ivory (AllocEffects s3) ()) -> CoroutineBody (Stored Uint8)
mavlinkReceiver onFail onSuccess = CoroutineBody $ \ yield -> do
  forever $ noBreak $ do
    forever $ do
      b <- deref =<< yield
      when (b ==? mavlink_STX) breakOut

    state <- local $ istruct [ crc .= ival crc_init_v ]
    let updateCRC ref = do
          refCopy ref =<< yield
          b <- deref ref
          call_ crc_accumulate b (state ~> crc)
          return b

    len <- updateCRC (state ~> paylen)
    _ <- updateCRC (state ~> seqnum)
    _ <- updateCRC (state ~> sysid)
    _ <- updateCRC (state ~> compid)
    mid <- updateCRC (state ~> msgid)

    expectedLength <- deref (addrOf msgLengths ! toIx mid)
    ifte_ (len /=? expectedLength) (noReturn onFail) $ do
      for (toIx len) $ \ i -> do
        updateCRC (state ~> payload ! i)

      crcExtra <- deref (addrOf msgCRCExtras ! toIx mid)
      call_ crc_accumulate crcExtra (state ~> crc)

      (expectedLo, expectedHi) <- crc_lo_hi (state ~> crc)
      crcLo <- deref =<< yield
      ifte_ (crcLo /=? expectedLo) (noReturn onFail) $ do
        crcHi <- deref =<< yield
        ifte_ (crcHi /=? expectedHi) (noReturn onFail) $ do
          noReturn $ onSuccess $ constRef state
