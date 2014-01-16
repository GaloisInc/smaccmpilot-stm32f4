{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- Altitude controller based off work by Anton Babushkin <rk3dov@gmail.com> for
-- the PX4 Autopilot Project
-- https://github.com/PX4/Firmware/tree/master/src/modules/multirotor_pos_control

module SMACCMPilot.Flight.Control.Altitude
  ( AltitudeControl(..)
  , taskAltitudeControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.PID (fconstrain)
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
import qualified SMACCMPilot.Flight.Types.ControlSetpoint as SP
import           SMACCMPilot.Flight.Types.UserInput       ()
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO
import qualified SMACCMPilot.Flight.Types.ControlSource as CS
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data AltitudeControl =
   AltitudeControl
    { alt_init   :: forall eff   . Ivory eff ()
    , alt_update :: forall eff s1 s2 s3 s4
                 . Ref s1 (Struct "sensors_result")
                -> Ref s2 (Struct "userinput_result")
                -> Ref s3 (Struct "control_setpoint")
                -> Ref s4 (Struct "control_law")
                -> IFloat -- dt, seconds
                -> Ivory eff ()
    , alt_output :: forall eff s . Ref s (Struct "controloutput")
                              -> Ivory eff ()
    }

taskAltitudeControl :: AltitudeParams ParamReader
                    -> DataSource (Struct "alt_control_dbg")
                    -> Task p AltitudeControl
taskAltitudeControl params altDbgSrc = do
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
  at_setpt <- taskLocal "at_setpt"
  -- setpoint for manual passthrough control, and a boolean indicating
  -- whether autothrottle is valid or not
  ui_setpt <- taskLocal "ui_setpt"
  at_enabled <- taskLocal "at_enabled"

  altDbgWriter <- withDataWriter altDbgSrc "alt_control_dbg"

  -- state is saved in a struct, to be marshalled into a mavlink message
  state_dbg        <- taskLocal "state_debug"
  let named n = "alt_ctl_" ++ n ++ "_" ++ show uniq
      proc_alt_update :: Def('[ Ref s1 (Struct "sensors_result")
                              , Ref s2 (Struct "userinput_result")
                              , Ref s3 (Struct "control_setpoint")
                              , Ref s4 (Struct "control_law")
                              , IFloat
                              ]:->())
      proc_alt_update = proc (named "update") $ \sens ui ctl_sp cl dt -> body $ do

          thr_mode <- deref (cl ~> CL.thr_mode)
          armed_mode <- deref (cl ~> CL.armed_mode)
          enabled <- assign ((thr_mode ==? TM.autothrottle)
                         .&& (armed_mode ==? A.armed))

          store ui_setpt =<< manual_throttle ui
          store at_enabled enabled

          -- Update estimators
          tt_update throttle_tracker ui enabled

          sensor_alt  <- deref (sens ~> S.baro_alt)
          sensor_time <- deref (sens ~> S.baro_time)
          ae_measurement alt_estimator sensor_alt sensor_time

          when enabled $ do
            autothr_source <- deref (cl ~> CL.autothr_source)
            vz_control <- cond
              [ autothr_source ==? CS.ui ==> do
                  -- update setpoint ui
                  tui_update      ui_control ui dt
                  -- update position controller
                  (ui_alt, ui_vz) <- tui_setpoint ui_control
                  vz_ctl <- pos_pid_calculate position_pid ui_alt ui_vz dt
                  store (state_dbg ~> A.pos_rate_setp) vz_ctl
                  return vz_ctl
              , autothr_source ==? CS.nav ==> do
                  -- update setpoint ui
                  tui_reset ui_control
                  -- update position controller
                  sp_alt <- deref (ctl_sp ~> SP.altitude)
                  sp_rate <- deref (ctl_sp ~> SP.alt_rate)
                  vz_ctl <- pos_pid_calculate position_pid sp_alt 0 dt
                  -- Only limit rate if a nonzero positive number
                  ifte (sp_rate >? 0)
                    (do vz_ctl' <- call fconstrain (-1*sp_rate) sp_rate vz_ctl
                        store (state_dbg ~> A.pos_rate_setp) vz_ctl'
                        return vz_ctl')
                    (do store (state_dbg ~> A.pos_rate_setp) vz_ctl
                        return vz_ctl)
              ]
            -- Manage thrust pid integral reset, if required.
            (reset_integral, new_integral) <- tt_reset_to throttle_tracker
            when reset_integral $ do
              store (state_dbg ~> A.thrust_i_reset) new_integral
              thrust_pid_set_integral thrust_pid new_integral
            -- calculate thrust pid
            uncomp_thr_setpt <- thrust_pid_calculate thrust_pid vz_control dt
            r22              <- sensorsR22 sens
            setpt <- assign ((throttleR22Comp r22) * uncomp_thr_setpt)
            store at_setpt ((setpt >? 1.0) ? (1.0, setpt))

          unless enabled $ do
            tui_reset       ui_control
            -- Reset derivative tracking when not using thrust controller
            thrust_pid_init thrust_pid

          tui_write_debug ui_control state_dbg
          ae_write_debug alt_estimator state_dbg
          thrust_pid_write_debug thrust_pid state_dbg
          writeData altDbgWriter (constRef state_dbg)
      proc_alt_output :: Def('[Ref s (Struct "controloutput")]:->())
      proc_alt_output = proc (named "output") $ \out -> body $ do
        at <- deref at_setpt
        en <- deref at_enabled
        ui <- deref ui_setpt
        ifte_ en
          (store (out ~> CO.throttle) at)
          (store (out ~> CO.throttle) ui)

  taskModuleDef $ do
    incl proc_alt_update
    incl proc_alt_output
  return AltitudeControl
    { alt_init = do
        store at_enabled false
        ae_init alt_estimator
        tt_init throttle_tracker
        thrust_pid_init thrust_pid
    , alt_update = call_ proc_alt_update
    , alt_output = call_ proc_alt_output
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


