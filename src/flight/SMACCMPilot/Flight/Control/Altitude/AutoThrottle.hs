{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- Altitude controller based off work by Anton Babushkin <rk3dov@gmail.com> for
-- the PX4 Autopilot Project
-- https://github.com/PX4/Firmware/tree/master/src/modules/multirotor_pos_control

module SMACCMPilot.Flight.Control.Altitude.AutoThrottle where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker
import           SMACCMPilot.Flight.Control.Altitude.ThrustPID
import           SMACCMPilot.Flight.Control.Altitude.PositionPID
import           SMACCMPilot.Flight.Control.Altitude.ThrottleUI

import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.ThrottleMode    as TM
import qualified SMACCMPilot.Flight.Types.ArmedMode       as A
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import           SMACCMPilot.Flight.Types.UserInput       ()
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data AutoThrottle =
  AutoThrottle
    { at_init   :: forall eff   . Ivory eff ()
    , at_update :: forall eff s1 s2 s3
                 . Ref s1 (Struct "sensors_result")
                -> Ref s2 (Struct "userinput_result")
                -> Ref s3 (Struct "control_law")
                -> IFloat -- dt, seconds
                -> Ivory eff ()
    , at_output :: forall eff   . Ivory eff IFloat
    }

taskAutoThrottle :: AltitudeParams ParamReader
                 -> DataWriter (Struct "alt_control_dbg")
                 -> Task p AutoThrottle
taskAutoThrottle params ahWriter = do
  uniq <- fresh
  -- Alt estimator filters noisy sensor into altitude & its derivative
  alt_estimator    <- taskAltEstimator
  -- Throttle tracker keeps track of steady-state throttle for transitioning
  -- into autothrottle mode
  throttle_tracker <- taskThrottleTracker
  -- Thrust PID controls altitude rate with thrust
  thrust_pid       <- taskThrustPid   (altitudeRateThrust params) alt_estimator
  -- Position PID controls altitude with altitude rate
  position_pid     <- taskPositionPid (altitudePosition   params) alt_estimator
  -- UI controls Position setpoint from user stick input
  ui_control       <- taskThrottleUI  (altitudeUI         params) alt_estimator
  -- setpoint: result of the whole autothrottle calculation
  thr_setpt        <- taskLocal "thr_setpt"
  -- state is saved in a struct, to be marshalled into a mavlink message
  state_dbg        <- taskLocal "state_debug"
  let proc_at_update :: Def('[ Ref s1 (Struct "sensors_result")
                             , Ref s2 (Struct "userinput_result")
                             , Ref s3 (Struct "control_law")
                             , IFloat
                             ]:->())
      proc_at_update = proc ("at_update_" ++ show uniq) $
        \sens ui cl dt -> body $ do

          thr_mode <- deref (cl ~> CL.thr_mode)
          armed_mode <- deref (cl ~> CL.armed_mode)
          enabled <- assign ((thr_mode ==? TM.autothrottle)
                         .&& (armed_mode ==? A.armed))

          -- Update estimators
          tt_update throttle_tracker ui enabled

          sensor_alt  <- deref (sens ~> S.baro_alt)
          sensor_time <- deref (sens ~> S.baro_time)
          ae_measurement alt_estimator sensor_alt sensor_time
          ae_write_debug alt_estimator state_dbg

          ui_update ui_control enabled ui dt
          ui_write_debug ui_control state_dbg

          when enabled $ do
            -- update position controller
            (ui_alt, ui_vz) <- ui_setpoint ui_control
            vz_control      <- pos_pid_calculate position_pid ui_alt ui_vz dt
            store (state_dbg ~> A.pos_rate_setp) vz_control

            -- Manage thrust pid integral reset, if required.
            (reset_integral, new_integral) <- tt_reset_to throttle_tracker
            when reset_integral $ do
              store (state_dbg ~> A.thrust_i_reset) new_integral
              thrust_pid_set_integral thrust_pid new_integral
            -- calculate thrust pid
            uncomp_thr_setpt <- thrust_pid_calculate thrust_pid vz_control dt
            thrust_pid_write_debug thrust_pid state_dbg
            r22              <- sensorsR22 sens
            setpt <- assign ((throttleR22Comp r22) * uncomp_thr_setpt)
            store thr_setpt ((setpt >? 1.0) ? (1.0, setpt))

          unless enabled $ do
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


