{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.AutoThrottle where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker
import           SMACCMPilot.Flight.Control.Altitude.ThrustPID
import           SMACCMPilot.Flight.Control.Altitude.PositionPID
import           SMACCMPilot.Flight.Control.Altitude.ThrottleUI
import           SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import           SMACCMPilot.Flight.Types.Armed
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import           SMACCMPilot.Flight.Types.UserInput       ()
import           SMACCMPilot.Flight.Types.FlightModeData
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data AutoThrottle =
  AutoThrottle
    { at_init   :: forall eff   . Ivory eff ()
    , at_update :: forall eff s1 s2 . Ref s1 (Struct "sensors_result")
                               -> Ref s2 (Struct "userinput_result")
                               -> FlightMode
                               -> ArmedMode
                               -> IFloat -- dt, seconds
                               -> Ivory eff ()
    , at_output :: forall eff   . Ivory eff IFloat
    }

taskAutoThrottle :: AltitudeParams ParamReader
                 -> DataWriter (Struct "alt_control_dbg")
                 -> Task p AutoThrottle
taskAutoThrottle params ahWriter = do
  uniq <- fresh
  alt_estimator    <- taskAltEstimator
  throttle_tracker <- taskThrottleTracker
  thrust_pid       <- taskThrustPid   (altitudeRateThrust params)
  position_pid     <- taskPositionPid (altitudePosition   params)
  ui_control       <- taskThrottleUI  (altitudeUI         params)
  thr_setpt        <- taskLocal "thr_setpt"
  state_dbg        <- taskLocal "state_debug"
  let proc_at_update :: Def('[ Ref s1 (Struct "sensors_result")
                             , Ref s2 (Struct "userinput_result")
                             , FlightMode
                             , ArmedMode
                             , IFloat
                             ]:->())
      proc_at_update = proc ("at_update_" ++ show uniq) $
        \sens ui mode armed dt -> body $ do
          -- Update estimators
          tt_update throttle_tracker ui mode armed

          sensor_alt  <- deref (sens ~> S.baro_alt)
          sensor_time <- deref (sens ~> S.baro_time)
          ae_measurement alt_estimator sensor_alt sensor_time
          ae_write_debug alt_estimator state_dbg

          (alt_est, vz_est) <- ae_state alt_estimator
          ui_update ui_control mode armed ui alt_est vz_est dt

          when (autoThrottleEnabled mode) $ do
            (ui_alt, ui_vz)   <- ui_setpoint ui_control
            vz_control        <- pos_pid_calculate position_pid ui_alt ui_vz alt_est dt

            -- Manage thrust pid integral reset, if required.
            (reset_integral, new_integral) <- tt_reset_to throttle_tracker
            when reset_integral $ do
              store (state_dbg ~> A.thrust_i_reset) new_integral
              thrust_pid_set_integral thrust_pid new_integral
            -- calculate thrust pid
            uncomp_thr_setpt <- thrust_pid_calculate thrust_pid vz_control vz_est dt
            thrust_pid_write_debug thrust_pid state_dbg
            r22              <- sensorsR22 sens
            setpt <- assign ((throttleR22Comp r22) * uncomp_thr_setpt)
            store thr_setpt ((setpt >? 1.0) ? (1.0, setpt))

          unless (autoThrottleEnabled mode) $ do
            -- Reset derivative tracking when not using thrust controller
            thrust_pid_init thrust_pid

          writeData ahWriter (constRef state_dbg)

  taskModuleDef $ incl proc_at_update
  return AutoThrottle
    { at_init = do
        ae_init alt_estimator
        tt_init throttle_tracker
        thrust_pid_init thrust_pid
    , at_update = call_ proc_at_update
    , at_output = deref thr_setpt
    }

autoThrottleEnabled :: FlightMode -> IBool
autoThrottleEnabled m = m ==? flightModeAltHold .|| m ==? flightModeAuto

-- | Calculate R22 factor, product of cosines of roll & pitch angles
sensorsR22 :: Ref s (Struct "sensors_result") -> Ivory eff IFloat
sensorsR22 sens = do
  pitch <- deref (sens ~> S.pitch)
  roll  <- deref (sens ~> S.roll)
  r22   <- assign (cos pitch * cos roll)
  return r22

-- | Throttle compensation factor (multiplicative) based on R22
--   Based on PX4 project code (98a43454 Anton Babushkin)
--   R22 is rotation matrix element {2,2}, equivelant to cos pitch * cos roll
throttleR22Comp :: IFloat -> IFloat
throttleR22Comp r22 =
  (r22 >? 0.8) ? ((1.0 / r22)
  ,(r22 >? 0.0) ? ((((1.0 / 0.8 - 1.0) / 0.8) * r22 + 1.0)
   ,(1.0)))


