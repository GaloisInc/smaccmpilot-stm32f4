{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.AutoThrottle where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.PID

import           SMACCMPilot.Flight.Types.Armed
import qualified SMACCMPilot.Flight.Types.Sensors       as SENS
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
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

taskAutoThrottle :: AltHoldParams ParamReader
                 -> DataWriter (Struct "alt_hold_state")
                 -> Task p AutoThrottle
taskAutoThrottle params ahWriter = do
  uniq <- fresh
  alt_estimator    <- taskAltEstimator
  throttle_tracker <- taskThrottleTracker
  thrust_pid       <- taskThrustPid (altHoldThrottleRate params)
  thr_setpt        <- taskLocal "thr_setpt"
  let proc_at_update :: Def('[ Ref s1 (Struct "sensors_result")
                             , Ref s2 (Struct "userinput_result")
                             , FlightMode
                             , ArmedMode
                             , IFloat
                             ]:->())
      proc_at_update = proc ("at_update_" ++ show uniq) $ \sens ui mode armed dt -> body $ do
          -- Update estimators
          tt_update throttle_tracker ui mode armed

          sensor_alt  <- deref (sens ~> SENS.baro_alt)
          sensor_time <- deref (sens ~> SENS.baro_time)
          ae_measurement alt_estimator sensor_alt sensor_time
          (alt_est, vz_est) <- ae_state alt_estimator

          vz_setpt <- assign 0 -- XXX, add outer loop after basic testing for vz=0

          when (autoThrottleEnabled mode) $ do
            -- Manage thrust pid integral reset, if required.
            (reset_integral, new_integral) <- tt_reset_to throttle_tracker
            when reset_integral $
              thrust_pid_set_integral thrust_pid (-1 * new_integral)
            -- calculate thrust pid
            uncomp_thr_setpt <- thrust_pid_calculate thrust_pid vz_setpt vz_est dt
            r22              <- sensorsR22 sens
            setpt <- assign ((throttleR22Comp r22) * uncomp_thr_setpt)
            -- XXX: logic for preventing overflow should go here 
            store thr_setpt ((setpt >? 1.0) ? (1.0, setpt))
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

data ThrottleTracker =
  ThrottleTracker
    { tt_init      :: forall eff   . Ivory eff ()
    , tt_update    :: forall eff s . Ref s (Struct "userinput_result")
                                  -> FlightMode -> ArmedMode -> Ivory eff ()
    , tt_reset_to  :: forall eff   . Ivory eff (IBool, IFloat)
    }

-- XXX completely unimplemented
taskThrottleTracker :: Task p ThrottleTracker
taskThrottleTracker = do
  uniq <- fresh
  return ThrottleTracker
    { tt_init = do
        return ()
    , tt_update = \ui fm armed -> do
        return ()
    , tt_reset_to = do
        return (false, 0)
    }

data ThrustPid =
  ThrustPid
    { thrust_pid_init :: forall eff . Ivory eff ()
    , thrust_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , thrust_pid_calculate :: forall eff . IFloat -- Setpoint
                                        -> IFloat -- Estimate
                                        -> IFloat -- dt, seconds
                                        -> Ivory eff IFloat
    }

-- XXX completely unimplemented. should Take the pid param reader as an argument, too
taskThrustPid :: PIDParams ParamReader -> Task p ThrustPid
taskThrustPid params = do
  uniq <- fresh
  tpid_state  <- taskLocal "thrustPidState"
  tpid_params <- taskLocal "thrustPidParams"
  let proc_pid_calculate :: Def('[IFloat, IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc ("thrust_pid_calculate" ++ show uniq) $
        \vel_sp vel_est _dt -> body $ do
          getPIDParams params tpid_params
          -- XXX should adjust params by dt so they are update rate independent
          error <- assign (vel_sp - vel_est)
          out <- call pid_update tpid_state (constRef tpid_params) error vel_est
          ret out
  taskModuleDef $ incl proc_pid_calculate
  return ThrustPid
    { thrust_pid_init = do
        call_ pid_reset tpid_state
    , thrust_pid_set_integral = \i ->
        store (tpid_state ~> pid_iState) i
    , thrust_pid_calculate = call proc_pid_calculate
    }

-- | Calculate R22 factor, product of cosines of roll & pitch angles
sensorsR22 :: Ref s (Struct "sensors_result") -> Ivory eff IFloat
sensorsR22 sens = do
  pitch     <- deref (sens ~> SENS.pitch)
  roll      <- deref (sens ~> SENS.roll)
  r22       <- assign (cos pitch * cos roll)
  return r22

-- | Throttle compensation factor (multiplicative) based on R22
--   Based on PX4 project code (98a43454 Anton Babushkin)
--   R22 is rotation matrix element {2,2}, equivelant to cos pitch * cos roll
throttleR22Comp :: IFloat -> IFloat
throttleR22Comp r22 =
  (r22 >? 0.8) ? ((1.0 / r22)
  ,(r22 >? 0.0) ? ((((1.0 / 0.8 - 1.0) / 0.8) * r22 + 1.0)
   ,(1.0)))


