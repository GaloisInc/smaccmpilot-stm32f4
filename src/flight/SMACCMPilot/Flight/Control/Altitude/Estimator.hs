{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.Estimator where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Flight.Control.Altitude.Filter

data AltEstimator =
  AltEstimator
    { ae_init        :: forall eff . Ivory eff ()
    , ae_measurement :: forall eff . IFloat -> Uint32 -> Ivory eff ()
    , ae_state       :: forall eff . Ivory eff (IFloat, IFloat)
    }

taskAltEstimator :: Task p AltEstimator
taskAltEstimator = do
  uniq          <- fresh
  prevAlt       <- taskLocal "prev_alt"
  prevAltTime   <- taskLocal "prev_alt_time"
  prevClimbRate <- taskLocal "prev_climb_rate"

  let measDef :: Def ('[IFloat, Uint32] :-> ())
      measDef = proc ("thrustEstimatorMeasure" ++ show uniq) $
        \alt_meas meas_time -> body $ do
          prev_time   <- deref prevAltTime
          when (meas_time /=? prev_time) $ do
            dt   <- assign ((safeCast (meas_time - prev_time)) / 1000.0)
            dalt <- lowPassDeriv'  2.0  dt alt_meas prevAlt
            rate <- lowPassFilter' 0.25 dt (dalt / dt) prevClimbRate
            store prevAltTime meas_time

  taskModuleDef $ incl measDef
  return AltEstimator
    { ae_init = do
        setNothing prevAlt
        setNothing prevClimbRate
    , ae_measurement = call_ measDef
    , ae_state = do
        a <- getMaybe (constRef prevAlt)       0
        r <- getMaybe (constRef prevClimbRate) 0
        return (a,r)
    }

