{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Datalink.RadioData
  ( radioDataHandler
  ) where

import qualified Data.Char as C (ord)

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified Ivory.HXStream                     as H
import qualified SMACCMPilot.Flight.Types.RadioStat as RS
import           SMACCMPilot.Flight.Types.RadioInfo()
import qualified SMACCMPilot.Communications         as C

radioDataHandler :: (SingI n0, SingI n1)
                 => ChannelEmitter n0 (Struct "radio_stat")
                 -> ChannelEmitter n1 (Struct "radio_info")
                 -> Task p H.FrameHandler
radioDataHandler statout infoout = do
  successCtr     <- taskLocalInit "rs_success" (ival (0::Uint32))
  failCtr        <- taskLocalInit "rs_fail"    (ival (0::Uint32))
  msgType        <- taskLocal     "rs_msgtype"
  msgLen         <- taskLocal     "rs_len"
  (msgBuf :: Ref Global (Array 48 (Stored Uint8))) <- taskLocal "rs_buf"
  return $ H.mkFrameHandler H.ScopedFrameHandler
    { H.fhTag = C.radioDataTag
    , H.fhBegin = do
        store msgType mtype_unknown
        store msgLen (0 :: Sint32)
        arrayMap $ \ix -> store (msgBuf ! ix) 0
    , H.fhData = \v offs -> do
        mt <- deref msgType
        cond_
          [ (offs >=? 48) ==> store msgType mtype_fail
          , (offs ==? 0) ==> cond_
                [ (v ==? (charUint8 'B')) ==> store msgType mtype_stat
                , (v ==? (charUint8 'A')) ==> store msgType mtype_info
                ]
          , (mt ==? mtype_fail) ==> return ()
          , (mt ==? mtype_stat) ==> do
             ifte_ (offs <? 22)
                   (store (msgBuf ! (toIx offs)) v >> store msgLen offs)
                   (store msgType mtype_fail)
          , (mt ==? mtype_info) ==> cond_
              [ ((offs ==? 1) .&& (v ==? (charUint8 'T')))  ==> return ()
              , ((offs ==? 2) .&& (v ==? (charUint8 'I')))  ==> return ()
              , ((offs ==? 3) .&& (v ==? (charUint8 '0')))  ==> return ()
              , ((offs ==? 4) .&& (v ==? (charUint8 '\n'))) ==> return ()
              , (offs >?  4) ==> (   store (msgBuf ! (toIx (offs - 4))) v
                                  >> store msgLen offs)
              , true ==> store msgType mtype_fail
              ]
          ]
    , H.fhEnd = do
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
  let d ix   = deref (raw ! (fromIntegral ix))
  derefs    <- mapM d [1..20]
  let val i  = derefs !! i
  local $ istruct
    [ RS.sik       .= ival   true
    , RS.loc_rssi  .= ival   (val 0)
    , RS.loc_noise .= ival   (val 1)
    , RS.loc_rxctr .= ival16 (val 2) (val 3)
    , RS.rem_rssi  .= ival   (val 4)
    , RS.rem_noise .= ival   (val 5)
    , RS.rem_rxctr .= ival16 (val 6) (val 7)
    , RS.tx_err    .= ival16 (val 8) (val 9)
    , RS.rx_err    .= ival16 (val 10) (val 11)
    , RS.tx_ovf    .= ival16 (val 12) (val 13)
    , RS.rx_ovf    .= ival16 (val 14) (val 15)
    , RS.ecc_errs  .= ival16 (val 16) (val 17)
    , RS.ecc_pkts  .= ival16 (val 18) (val 19)
    ]

mlen_info :: Sint32
mlen_info = 0 -- XXX
unpackInfo :: (GetAlloc eff ~ Scope s)
           => Ref Global (Array 48 (Stored Uint8))
           -> Ivory eff (Ref (Stack s) (Struct "radio_info"))
unpackInfo _raw = local (istruct []) -- XXX

charUint8 :: Char -> Uint8
charUint8 = fromIntegral . C.ord

ival16 :: Uint8 -> Uint8 -> Init (Stored Uint16)
ival16 msb lsb = ival (((safeCast msb) * 256) + (safeCast lsb))


