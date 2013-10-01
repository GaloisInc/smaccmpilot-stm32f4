{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Datalink.RadioData
  ( radioDataTag
  , radioDataHandler
  ) where

import qualified Data.Char as C (ord)

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified Ivory.HXStream                     as H
import qualified SMACCMPilot.Flight.Types.RadioStat as RS
import qualified SMACCMPilot.Flight.Types.RadioInfo as RI

radioDataTag :: Uint8
radioDataTag = 1

radioDataHandler :: (SingI n, SingI m)
                 => ChannelEmitter n (Struct "radio_stat")
                 -> ChannelEmitter m (Struct "radio_info")
                 -> Task p FrameHandler
radioDataHandler statout infoout = do
  successCtr     <- taskLocalInit "rs_success" (ival (0::Uint32))
  failCtr        <- taskLocalInit "rs_fail"    (ival (0::Uint32))
  msgType        <- taskLocal     "rs_msgtype"
  msgLen         <- taskLocal     "rs_len"
  (msgBuf :: Ref Global (Array 48 (Stored Uint8))) <- taskLocal "rs_buf"
  return $ FrameHandler $ ScopedFrameHandler
    { fh_tag = radioDataTag
    , fh_begin = do
        store msgType mtype_unknown
        store msgLen (0 :: Sint32)
        arrayMap $ \ix -> store (msgBuf ! ix) 0
    , fh_data = \v offs -> do
        mt <- deref msgType
        cond_
          [ (offs >=? 48) ==>
              store msgType mtype_fail
          , (offs ==? 0) ==> cond_
                [ (v ==? (charUint8 'B')) ==> store msgType mtype_stat
                , (v ==? (charUint8 'A')) ==> store msgType mtype_info
                ]
          , (mt ==? mtype_fail) ==>
              return ()
          , (mt ==? mtype_stat) ==> do
             ifte_ (offs <? 22)
                   (store (msgBuf ! (toIx offs)) v >> store msgLen offs)
                   (store msgType mtype_fail)
          , (mt ==? mtype_info) ==> cond_
              [ ((offs ==? 1) .&& (v ==? (charUint8 'T')))  ==> return ()
              , ((offs ==? 2) .&& (v ==? (charUint8 'I')))  ==> return ()
              , ((offs ==? 3) .&& (v ==? (charUint8 '0')))  ==> return ()
              , ((offs ==? 4) .&& (v ==? (charUint8 '\n'))) ==> return ()
              , (offs >?  4) ==> ( store (msgBuf ! (toIx (offs - 4))) v
                                >> store msgLen offs)
              , true ==> store msgType mtype_fail
              ]
          ]
    , fh_end = do
        mt <- deref msgType
        len <- deref msgLen
        cond_
          [ (mt ==? mtype_info .&& len ==? mlen_info) ==> do
              i <- unpackInfo msgBuf
              emit_ infoout (constRef i)
              successCtr %= (+1)
          , (mt ==? mtype_stat .&& len ==? mlen_stat) ==> do
              s <- unpackStat msgBuf
              emit_ statout (constRef s)
              successCtr %= (+1)
          , true ==> failCtr %= (+1)
          ]
    }

mtype_unknown, mtype_stat, mtype_info, mtype_fail :: Uint8
mtype_unknown = 0
mtype_stat    = 1
mtype_info    = 2
mtype_fail    = 3

mlen_stat :: Sint32
mlen_stat = 20
unpackStat :: (GetAlloc eff ~ Scope s)
            => Ref Global (Array 48 (Stored Uint8))
            -> Ivory eff (Ref (Stack s) (Struct "radio_stat"))
unpackStat raw = do
  -- This is not brilliant code but it gets the job done today.
  -- skip b0, should be 'B'
  b1   <- deref (raw ! 1)
  b2   <- deref (raw ! 2)
  b3   <- deref (raw ! 3)
  b4   <- deref (raw ! 4)
  b5   <- deref (raw ! 5)
  b6   <- deref (raw ! 6)
  b7   <- deref (raw ! 7)
  b8   <- deref (raw ! 8)
  b9   <- deref (raw ! 9)
  b10  <- deref (raw ! 10)
  b11  <- deref (raw ! 11)
  b12  <- deref (raw ! 12)
  b13  <- deref (raw ! 13)
  b14  <- deref (raw ! 14)
  b15  <- deref (raw ! 15)
  b16  <- deref (raw ! 16)
  b17  <- deref (raw ! 17)
  b18  <- deref (raw ! 18)
  b19  <- deref (raw ! 19)
  b20  <- deref (raw ! 20)
  local $ istruct
    [ RS.sik       .= ival   true
    , RS.loc_rssi  .= ival   b1
    , RS.loc_noise .= ival   b2
    , RS.loc_rxctr .= ival16 b3 b4
    , RS.rem_rssi  .= ival   b5
    , RS.rem_noise .= ival   b6
    , RS.rem_rxctr .= ival16 b7 b8
    , RS.tx_err    .= ival16 b9 b10
    , RS.rx_err    .= ival16 b11 b12
    , RS.tx_ovf    .= ival16 b13 b14
    , RS.rx_ovf    .= ival16 b15 b16
    , RS.ecc_errs  .= ival16 b17 b18
    , RS.ecc_pkts  .= ival16 b19 b20
    ]

mlen_info :: Sint32
mlen_info = 0 -- XXX
unpackInfo :: (GetAlloc eff ~ Scope s)
           => Ref Global (Array 48 (Stored Uint8))
           -> Ivory eff (Ref (Stack s) (Struct "radio_info"))
unpackInfo raw = local (istruct []) -- XXX

charUint8 :: Char -> Uint8
charUint8 = fromIntegral . C.ord

ival16 :: Uint8 -> Uint8 -> Init (Stored Uint16)
ival16 msb lsb = ival (((safeCast msb) * 256) + (safeCast lsb))


