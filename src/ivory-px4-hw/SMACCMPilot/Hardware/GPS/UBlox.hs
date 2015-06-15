{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.GPS.UBlox
  ( ubloxGPSTower
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import SMACCMPilot.Hardware.GPS.Types
import SMACCMPilot.Hardware.GPS.UBlox.Types

ubloxGPSTower :: ChanOutput (Stored Uint8)
              -> ChanInput  (Struct "position")
              -> Tower e ()
ubloxGPSTower istream ostream = do
  towerModule  gpsTypesModule
  towerDepends gpsTypesModule
  monitor "ubloxGPS" $ do

    decodestate <- stateInit "decodestate" (ival ubx_idle)
    pktClass <- state "pktClass"
    pktId    <- state "pktId"
    (pktLen   :: Ref Global (Stored Uint16)) <- state "pktLen"
    (payOffs  :: Ref Global (Stored Uint16)) <- state "payOffs"
    (payload  :: Ref Global (Array 52 (Stored Uint8))) <- state "payload"
    (position :: Ref Global (Struct "position")) <- state "position_1"
    (dstate   :: Ref Global (Stored Uint8)) <- state "decode_state"

    monitorModuleDef $ do
      incl decode
      incl unpackS4

    ck_a <- state "ck_a"
    ck_b <- state "ck_b"

    let newframe = do
          store ck_a 0
          store ck_b 0
          arrayMap $ \i -> store (payload ! i) 0
        cksum i = do
          a <- deref ck_a
          b <- deref ck_b
          -- Sum depends on discarding overflowed bits of uint8,
          -- so we cast up a size, do the addition there, and then
          -- just take the lower 8 bits of the result.
          let a_16 :: Uint16 = safeCast a
              b_16 :: Uint16 = safeCast b
              i_16 :: Uint16 = safeCast i
          store ck_a (lbits (a_16+i_16))
          store ck_b (lbits (a_16+b_16+i_16))
    handler systemInit "init" $ do
      e <- emitter ostream 1
      callback $ const $ do
        t <- getTime
        store (position ~> time) t
        store (position ~> fix) fix_none
        emit e (constRef position)
    handler istream "istream" $ do
      e <- emitter ostream 1
      callback $ \cref -> do
        c <- deref cref
        s <- deref decodestate
        cond_
          [ (s ==? ubx_idle .&& c ==? 0xB5) ==> store decodestate ubx_sync
          , (s ==? ubx_sync .&& c ==? 0x62) ==> store decodestate ubx_class
          , (s ==? ubx_class) ==> do
              newframe
              cksum c
              store pktClass c
              store decodestate ubx_id
          , (s ==? ubx_id) ==> do
              cksum c
              store pktId c
              store decodestate ubx_len1
          , (s ==? ubx_len1) ==> do
              cksum c
              store pktLen (safeCast c)
              store decodestate ubx_len2
          , (s ==? ubx_len2) ==> do
              cksum c
              l <- deref pktLen
              store pktLen (((safeCast c)*256)+l)
              store payOffs 0
              store decodestate ubx_payload
          , (s ==? ubx_payload) ==> do
              off <- deref payOffs
              len <- deref pktLen
              ifte_ (off <? len)
                (do cksum c
                    when (off <? 52) $ store (payload ! (toIx off)) c
                    store payOffs (off+1))
                (do a <- deref ck_a
                    ifte_ (c ==? a)
                          (store decodestate ubx_cksum)
                          (store decodestate ubx_idle)
                )
          , (s ==? ubx_cksum) ==> do
              b <- deref ck_b
              when (c ==? b) $ do
                cl  <- deref pktClass
                i   <- deref pktId
                len <- deref payOffs
                call_ decode dstate cl i payload len position
                d <- deref dstate
                when (d ==? decode_complete) $ do
                  t <- getTime
                  store (position ~> time) t
                  emit e (constRef position)
                  store dstate decode_none
              store decodestate ubx_idle
          , true ==> store decodestate ubx_idle
          ]

decode_none, decode_got_sol, decode_got_posllh, decode_got_velned, decode_complete :: Uint8
decode_none       = 0
decode_got_sol    = 1
decode_got_posllh = 2
decode_got_velned = 4
decode_complete   = decode_got_sol .| decode_got_posllh .| decode_got_velned

class_nav :: Uint8
class_nav = 0x01
pktid_sol, pktid_posllh, pktid_velned :: Uint8
pktid_posllh = 0x02
pktid_sol    = 0x06
pktid_velned = 0x12

decode :: Def ('[ Ref s1 (Stored Uint8)
                , Uint8 -- class
                , Uint8 -- id
                , Ref s2 (Array 52 (Stored Uint8)) -- payload
                , Uint16 -- len
                , Ref s3 (Struct "position")
                ] :->())
decode = proc "decode" $ \ decodestate pktclass pktid payload len out -> body $ do
  s <- deref decodestate
  cond_
    [ pktclass ==? class_nav .&& pktid ==? pktid_posllh .&& len ==? 28 ==> do
        store decodestate (s .| decode_got_posllh)
        unpack_posllh payload out
    , pktclass ==? class_nav .&& pktid ==? pktid_sol    .&& len ==? 52 ==> do
        store decodestate (s .| decode_got_sol)
        unpack_sol payload out
    , pktclass ==? class_nav .&& pktid ==? pktid_velned .&& len ==? 36 ==> do
        store decodestate (s .| decode_got_velned)
        unpack_velned payload out
    ]

unpack_posllh :: Ref s1 (Array 52 (Stored Uint8))
              -> Ref s2 (Struct "position")
              -> Ivory eff ()
unpack_posllh payload out = do
  p_lat <- call unpackS4 payload 4
  store (out ~> lat) p_lat
  p_lon <- call unpackS4 payload 8
  store (out ~> lon) p_lon
  p_alt <- call unpackS4 payload 12
  store (out ~> alt) p_alt

unpackS4 :: Def ('[ Ref s1 (Array 52 (Stored Uint8))
                  , Ix 52
                  ] :-> Sint32)
unpackS4 = proc "unpackS4" $ \a off -> body $ do
  b1 <- deref (a ! (off+0))
  b2 <- deref (a ! (off+1))
  b3 <- deref (a ! (off+2))
  b4 <- deref (a ! (off+3))
  r :: Uint32 <- assign ( (256 * 256 * 256 * (safeCast b4))
                        + (256 * 256 * (safeCast b3))
                        + (256 * (safeCast b2))
                        + (safeCast b1))
  negr   :: Uint64 <- assign (0x100000000 - (safeCast r))
  s_negr :: Sint64 <- assign (signCast negr)
  ret ((r <? 0x7FFFFFFF) ? (signCast r, -1 * (castWith 0 s_negr)))


unpack_sol :: Ref s1 (Array 52 (Stored Uint8))
           -> Ref s2 (Struct "position")
           -> Ivory eff ()
unpack_sol payload out = do
  gpsfix <- deref (payload ! 10)
  flags  <- deref (payload ! 11)
  let gpsFixOk = (flags .& 0x01) >? 0
      fix_decoded = (gpsfix ==? 0x03) ? (fix_3d
                     ,(gpsfix ==? 0x02) ? (fix_2d
                       ,fix_none))
  store (out ~> fix) (gpsFixOk ? (fix_decoded,fix_none))
  pdop_l <- deref (payload ! 44)
  pdop_h <- deref (payload ! 45)
  (pdop :: Uint16) <- assign $ safeCast pdop_l + (256 * safeCast pdop_h)
  store (out ~> dop) (0.01 * safeCast pdop)
  numsv <- deref (payload ! 47)
  store (out ~> num_sv) numsv

unpack_velned :: Ref s1 (Array 52 (Stored Uint8))
              -> Ref s2 (Struct "position")
              -> Ivory eff ()
unpack_velned payload out = do
  p_vnorth   <- call unpackS4 payload 4
  p_veast    <- call unpackS4 payload 8
  p_vdown    <- call unpackS4 payload 12
  -- Technically vground is an Uint32. But we'd have to be
  -- going over 0.07*speed of light for it to matter.
  p_vground  <- call unpackS4 payload 16
  int_head   <- call unpackS4 payload 24
  store (out ~> vnorth)  p_vnorth
  store (out ~> veast)   p_veast
  store (out ~> vdown)   p_vdown
  store (out ~> vground) (signCast p_vground)
  store (out ~> heading) ((safeCast int_head)/100000.0)

