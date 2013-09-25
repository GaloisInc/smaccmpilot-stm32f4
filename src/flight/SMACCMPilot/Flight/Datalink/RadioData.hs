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
import Ivory.HXStream

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
          , (mt ==? mtype_fail) ==>
              return ()
          , (offs ==? 0) ==>
              unless (v ==? (charUint8 'B')) (store msgType mtype_fail)
          ]
    , fh_end = do
        mt <- deref msgType
        len <- deref msgLen
        cond_
          [ (mt ==? mtype_info .&& len ==? mlen_info) ==> do
              decoded_info <- decodeInfo msgBuf
              emit_ infoout (constRef decoded_info)
              successCtr %= (+1)
          , (mt ==? mtype_stat .&& len ==? mlen_stat) ==> do
              decoded_stat <- decodeStat msgBuf
              emit_ statout (constRef decoded_stat)
              successCtr %= (+1)
          , true ==> failCtr %= (+1)
          ]
    }

mtype_unknown, mtype_stat, mtype_info, mtype_fail :: Uint8
mtype_unknown = 0
mtype_stat    = 1
mtype_info    = 2
mtype_fail    = 3

mlen_info = 0 -- XXX
decodeInfo :: (GetAlloc eff ~ Scope s)
            => Ref Global (Array 48 (Stored Uint8))
            -> Ivory eff (Ref (Stack s) (Struct "radio_info"))
decodeInfo raw = local (istruct []) -- XXX

mlen_stat = 0 -- XXX
decodeStat :: (GetAlloc eff ~ Scope s)
           => Ref Global (Array 48 (Stored Uint8))
           -> Ivory eff (Ref (Stack s) (Struct "radio_stat"))
decodeStat raw = local (istruct []) -- XXX

charUint8 :: Char -> Uint8
charUint8 = fromIntegral . C.ord

