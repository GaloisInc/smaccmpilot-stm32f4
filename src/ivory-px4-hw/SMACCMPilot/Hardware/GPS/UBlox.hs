{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.GPS.UBlox
  ( ubloxGPSTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import SMACCMPilot.Hardware.GPS.UBlox.Types

ubloxGPSTask :: (SingI n, SingI m, SingI o)
             => ChannelSource n (Stored Uint8)
             -> ChannelSink   m (Stored Uint8)
             -> ChannelSource o (Stored Uint8) -- Temporary
             -> Task p ()
ubloxGPSTask osrc isnk orxd = do
  ostream <- withChannelEmitter osrc "ostream"
  istream <- withChannelEvent   isnk "istream"
  rxed    <- withChannelEmitter orxd "orxed"

  state    <- taskLocalInit "state" (ival ubx_idle)
  pktClass <- taskLocal "pktClass"
  pktId    <- taskLocal "pktId"
  (pktLen :: Ref Global (Stored Uint16)) <- taskLocal "pktLen"
  (payOffs :: Ref Global (Stored Uint16)) <- taskLocal "payOffs"

  ck_a <- taskLocal "ck_a"
  ck_b <- taskLocal "ck_a"
  let newcksum = store ck_a 0 >> store ck_b 0
      cksum i = do
        a <- deref ck_a
        b <- deref ck_b
        store ck_a (a+i)
        store ck_b (a+b+i)
  onEventV istream $ \c -> do
    s <- deref state
    cond_
      [ (s ==? ubx_idle .&& c ==? 0xB5) ==> store state ubx_sync
      , (s ==? ubx_sync .&& c ==? 0x62) ==> store state ubx_class
      , (s ==? ubx_class) ==> do
          newcksum
          cksum c
          store pktClass c
          store state ubx_id
      , (s ==? ubx_id) ==> do
          cksum c
          store pktId c
          store state ubx_len1
      , (s ==? ubx_len1) ==> do
          cksum c
          store pktLen (safeCast c)
          store state ubx_len2
      , (s ==? ubx_len2) ==> do
          cksum c
          l <- deref pktLen
          store pktLen (((safeCast c)*256)+l)
          store payOffs 0
          store state ubx_payload
      , (s ==? ubx_payload) ==> do
          off <- deref payOffs
          len <- deref pktLen
          ifte_ (off <? len)
            (cksum c >> store payOffs (off+1))
            -- XXX store payload somewhere
            (do a <- deref ck_a
                ifte_ (c ==? a)
                      (store state ubx_cksum)
                      (store state ubx_idle)
            )
      , (s ==? ubx_cksum) ==> do
          b <- deref ck_b
          when (c ==? b) $ emit_ rxed (constRef pktId)
          store state ubx_idle
      , true ==> store state ubx_idle
      ]

