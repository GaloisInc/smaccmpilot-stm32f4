{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.Mavlink.Receive where

import Prelude hiding (id,fail)

import Ivory.Language
import Ivory.Stdlib
import SMACCMPilot.Mavlink.CRC
import SMACCMPilot.Mavlink.Messages (messageLensCRCs)

[ivory|

struct mavlink_receive_state
  { status   :: Stored Uint8
  ; offs     :: Stored Uint8
  ; paylen   :: Stored Uint8
  ; seqnum   :: Stored Uint8
  ; sysid    :: Stored Uint8
  ; compid   :: Stored Uint8
  ; msgid    :: Stored Uint8
  ; payload  :: Array 255 (Stored Uint8)
  ; crc      :: Stored CRC
  }
|]

mavlinkReceiveStateModule :: Module
mavlinkReceiveStateModule = package "mavlink_receive_state" $
  defStruct (Proxy :: Proxy "mavlink_receive_state")


status_IDLE, status_ACTIVE, status_GOTMSG, status_FAIL :: Uint8
status_IDLE   = 0
status_ACTIVE = 1
status_GOTMSG = 2
status_FAIL = 3

mavlink_STX :: Uint8
mavlink_STX = 254

mavlinkReceiveReset :: Ref s1 (Struct "mavlink_receive_state") -> Ivory eff ()
mavlinkReceiveReset state = store (state ~> status) status_IDLE

mavlinkReceiveByte :: Ref s1 (Struct "mavlink_receive_state")
                   -> Uint8 -> Ivory eff ()
mavlinkReceiveByte state b = do
  s <- deref (state ~> status)
  unless (s ==? status_GOTMSG) -- Do nothing until statemachine cleared
                               -- externally
    (ifte_ (s ==? status_IDLE) -- When idle, look for STX
      (when (b ==? mavlink_STX)
        (beginActive state)) -- start state machine when got STX
      (active state b)) -- when running
  where

  beginActive :: Ref s1 (Struct "mavlink_receive_state") -> Ivory eff ()
  beginActive s = do
    store (s ~> status) status_ACTIVE
    store (s ~> offs)   1
    store (s ~> crc)    crc_init_v


active :: Ref s1 (Struct "mavlink_receive_state") -> Uint8 -> Ivory eff ()
active state b = do
  o <- deref (state ~> offs)
  len <- deref (state ~> paylen)
  cond_
    [ (o ==? 1)       ==> (store (state ~> paylen) b >> continue)
    , (o ==? 2)       ==> (store (state ~> seqnum) b >> continue)
    , (o ==? 3)       ==> (store (state ~> sysid)  b >> continue)
    , (o ==? 4)       ==> (store (state ~> compid) b >> continue)
    , (o ==? 5)       ==> checkMsgID
    , (o <? len + 6)  ==> gotPayload o
    , (o ==? len + 6) ==> gotCRCLo -- crc1
    , (o ==? len + 7) ==> gotCRCHi -- crc1
    , true            ==> fail --- should be impossible!!
    ]

  where
  fail     = store (state ~> status) status_FAIL
  success  = store (state ~> status) status_GOTMSG
  continue = call_ crc_accumulate b (state ~> crc) >> incrOffs

  incrOffs = do
    o <- deref (state ~> offs)
    store (state ~> offs) (o + 1)

  checkMsgID = do
    l <- deref (state ~> paylen)
    ifte_ (msgValidLen b l)
      (store (state ~> msgid) b >> continue)
      fail

  gotPayload off = do
    assert (off >=? 6) -- state machine enforced
    assert (off <? arrayLen (state ~> payload)) -- uint8 type enforced
    store ((state ~> payload) ! (toIx (off - 6))) b
    continue

  gotCRCLo = do
    id <- deref (state ~> msgid)
    call_ crc_accumulate (msgCRCExtra id) (state ~> crc)
    (lo, _) <- crc_lo_hi (state ~> crc)
    ifte_ (lo ==? b)
      incrOffs
      fail

  gotCRCHi = do
    (_, hi) <- crc_lo_hi (state ~> crc)
    ifte_ (hi ==? b)
      success
      fail

msgValidLen :: Uint8 -> Uint8 -> IBool
msgValidLen id len = foldr aux false messageLensCRCs
  where
  aux (mid, (mlen, _)) c =
    (id ==? (fromIntegral mid) .&& len ==? (fromIntegral mlen)) ? (true, c)

msgCRCExtra :: Uint8 -> Uint8
msgCRCExtra id = foldr aux 0 messageLensCRCs
  where
  aux (mid, (_, mcrc)) c =
    (id ==? (fromIntegral mid)) ? (fromIntegral mcrc, c)


