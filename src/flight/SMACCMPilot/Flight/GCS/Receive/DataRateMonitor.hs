{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Receive.DataRateMonitor
  ( mkDataRateMonitor
  , DataRateMonitor(..)
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.DataRate  as D

data DataRateMonitor =
  DataRateMonitor
    { drm_on_success :: forall eff s . (Scope s ~ GetAlloc eff) => Ivory eff ()
    , drm_on_fail    :: forall eff s . (Scope s ~ GetAlloc eff) => Ivory eff ()
    , drm_report     :: forall eff s . (Scope s ~ GetAlloc eff) => Ivory eff ()
    }

mkDataRateMonitor :: (SingI n)
                  => ChannelSource n (Struct "data_rate_state")
                  -> Task p DataRateMonitor
mkDataRateMonitor dr_src = do
  millis <- withGetTimeMillis
  drEmitter <- withChannelEmitter dr_src "data_rate_chan"
  (drInfo :: Ref Global (Struct "data_rate_state")) <- taskLocal "dropInfo"
  return $ DataRateMonitor
    { drm_on_success = do
        -- XXX We need to have a story for messages that are parsed
        -- correctly but are not recognized by the system---one could
        -- launch a DoS with those, too.
        t <- getTimeMillis millis
        store (drInfo ~> D.lastSucc) t
    , drm_on_fail = do
        (drInfo ~> D.dropped) += 1
    , drm_report = do
        when false $ -- XXX INTENTIONALLY DEAD CODE:
          emit_ drEmitter (constRef drInfo)
    }

