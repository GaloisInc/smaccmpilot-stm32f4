{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Mavlink.Send where

import           Ivory.Language
import           Ivory.Stdlib

import           SMACCMPilot.Mavlink.CRC
import qualified SMACCMPilot.Communications as C

--------------------------------------------------------------------------------

[ivory|
struct mavlinkPacket
  { mav_array :: Array 80 (Stored Uint8) -- C. CommsecArray
  ; mav_size  :: Stored Uint8
  }
|]

--------------------------------------------------------------------------------

mavlinkChecksum ::
     (GetAlloc eff ~ Scope cs)
  => Uint8
  -> Uint8
  -> Ref s C.MAVLinkArray
  -> Ivory eff ()
mavlinkChecksum sz crcextra arr = do
  ck <- local (ival crc_init_v)
  for (toIx sz) $ \i ->
    -- mavlink doesn't use the magic number
    -- in header[0] for crc calculation.
    unless (i ==? 0) $ do
      b <- deref (arr ! i)
      call_ crc_accumulate b ck

  call_ crc_accumulate crcextra ck
  (lo, hi) <- crc_lo_hi ck

  assert (arrayLen arr >? (sz + 1))
  let szIx = toIx sz
  store (arr ! szIx) lo
  store (arr ! (szIx + 1)) hi

-- Magic constants
sysid, compid :: Uint8
sysid  = 1
compid = 0

const_MAVLINK_STX :: Uint8
const_MAVLINK_STX = 254

-- We assume the payload has already been copied into arr.
mavlinkSendWithWriter ::
  Def ('[ Uint8 -- msgID
        , Uint8 -- crcExtra
        , Uint8 -- payload length
        , Ref s (Stored Uint8) -- sequence number (use then increment)
        , Ref s (Struct "mavlinkPacket") -- what we'll put everything into
        ] :-> ())
mavlinkSendWithWriter =
  proc "mavlinkSendWithWriter"
  $ \msgId crcExtra payloadLen seqNum struct -> body
  $ do

    s      <- deref seqNum
    header <- local (
      iarray [ ival const_MAVLINK_STX
             , ival payloadLen
             , ival s
             , ival sysid
             , ival compid
             , ival msgId
             ] :: Init (Array 6 (Stored Uint8)))

    ifte_ (s ==? 255)
      (store seqNum 0)
      (store seqNum (s + 1))
    let sz = 6 + payloadLen :: Uint8
    let arr = struct ~> mav_array
    arrayCopy arr header 0 (arrayLen header)
    -- Calculate checksum and store in arr
    mavlinkChecksum sz crcExtra arr
    -- Store the total size (header + payload + CRCs)
    store (struct ~> mav_size) (sz + 2)

mavlinkSendModule :: Module
mavlinkSendModule = package "mavlinkSendModule" $ do
  depend mavlinkCRCModule
  incl mavlinkSendWithWriter
  defStruct (Proxy :: Proxy "mavlinkPacket")

--------------------------------------------------------------------------------
