{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.Estimator where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A

import SMACCMPilot.Flight.Control.Altitude.Filter

data AltEstimator =
  AltEstimator
    { ae_init        :: forall eff . Ivory eff ()
    , ae_measurement :: forall eff . IFloat -> ITime -> Ivory eff ()
    , ae_state       :: forall eff . Ivory eff (IFloat, IFloat)
    , ae_write_debug :: forall eff s . Ref s (Struct "alt_control_debug")
                                    -> Ivory eff ()
    }

monitorAltEstimator :: Monitor e AltEstimator
monitorAltEstimator = do
  prevAlt       <- state "prev_alt"
  prevAltTime   <- state "prev_alt_time"
  prevClimbRate <- state "prev_climb_rate"

  measName <- fmap showUnique $ freshname "thrustEstimatorMeasure"
  let measDef :: Def ('[IFloat, ITime] :-> ())
      measDef = proc measName $
        \alt_meas meas_time -> body $ do
          prev_time   <- deref prevAltTime
          when (meas_time /=? prev_time) $ do
            -- If the dtime exceeds capability of a sint32 or float, we're hosed
            let dtime = meas_time - prev_time
            (int_ms :: Sint32) <- assign $ castWith 0 $ toIMilliseconds dtime
            dt   <- assign ((safeCast int_ms) / 1000.0)
            dalt <- lowPassDeriv'  2.0  dt alt_meas prevAlt
            _rate <- lowPassFilter' 0.25 dt (dalt / dt) prevClimbRate
            store prevAltTime meas_time

  monitorModuleDef $ incl measDef
  return AltEstimator
    { ae_init = do
        setNothing prevAlt
        setNothing prevClimbRate
    , ae_measurement = call_ measDef
    , ae_state = do
        a <- getMaybe (constRef prevAlt)       0
        r <- getMaybe (constRef prevClimbRate) 0
        return (a,r)
    , ae_write_debug = \dbg -> do
        a <- getMaybe (constRef prevAlt)       0
        r <- getMaybe (constRef prevClimbRate) 0
        store (dbg ~> A.alt_est)      a
        store (dbg ~> A.alt_rate_est) r
    }

