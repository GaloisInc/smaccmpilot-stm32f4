{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Task to process airdata from the GCS.

module SMACCMPilot.Flight.Datalink.AirData
  ( airDataHandler ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import qualified Ivory.HXStream     as H
import qualified SMACCMPilot.Shared as S

--------------------------------------------------------------------------------

airDataHandler :: (SingI n)
               => ChannelEmitter n S.CommsecArray
               -> Task p H.FrameHandler
airDataHandler ostream = do
  decodedCtr     <- taskLocalInit "airdata_frames_decoded" (ival (0::Uint32))
  decoded        <- taskLocal     "airdata_decoded"
  overrun        <- taskLocal     "airdata_overrun"
  return $ H.mkFrameHandler H.ScopedFrameHandler
    { H.fhTag = S.airDataTag
    , H.fhBegin = do
        store overrun false
        arrayMap $ \ix -> store (decoded ! ix) 0
    , H.fhData = \v offs ->
        ifte_ (offs <? fromInteger S.commsecPkgSize)
              (store (decoded ! (toIx offs)) v)
              (store overrun true)
    , H.fhEnd = do
        ovr <- deref overrun
        unless ovr $ do
          emit_ ostream (constRef decoded)
          decodedCtr %= (+1)
    }

--------------------------------------------------------------------------------
