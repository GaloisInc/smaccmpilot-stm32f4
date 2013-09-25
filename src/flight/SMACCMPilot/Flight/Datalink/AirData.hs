{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Datalink.AirData
  ( AirDataFrame
  , AirDataFrameSize
  , airDataTag
  , airDataHandler
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import Ivory.HXStream

type AirDataFrameSize = 96
type AirDataFrame = Array AirDataFrameSize (Stored Uint8)
-- Type synonyms of naturals are broken with ghc 7.6.2, so we need this:
airDataFrameSize :: Sint32
airDataFrameSize = fromIntegral (fromSing (sing :: Sing 96))

airDataTag :: Uint8
airDataTag = 0

airDataHandler :: (SingI n)
               => ChannelEmitter n AirDataFrame
               -> Task p FrameHandler
airDataHandler ostream = do
  decodedCtr     <- taskLocalInit "airdata_frames_decoded" (ival (0::Uint32))
  decoded        <- taskLocal     "airdata_decoded"
  overrun        <- taskLocal     "airdata_overrun"
  return $ FrameHandler $ ScopedFrameHandler
    { fh_tag = airDataTag
    , fh_begin = do
        store overrun false
        arrayMap $ \ix -> store (decoded ! ix) 0
    , fh_data = \v offs ->
        ifte_ (offs <? airDataFrameSize)
              (store (decoded ! (toIx offs)) v)
              (store overrun true)
    , fh_end = do
        ovr <- deref overrun
        unless ovr $ do
          emit_ ostream (constRef decoded)
          decodedCtr %= (+1)
    }

