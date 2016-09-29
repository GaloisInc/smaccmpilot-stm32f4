{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- Altitude controller based off work by Anton Babushkin <rk3dov@gmail.com> for
-- the PX4 Autopilot Project
-- https://github.com/PX4/Firmware/tree/master/src/modules/multirotor_pos_control

module SMACCMPilot.Flight.Control.Altitude
  ( AltitudeControl(..)
  , monitorAltitudeControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.PID (fconstrain, controlPIDModule)
import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker
import           SMACCMPilot.Flight.Control.Altitude.ThrustPID
import           SMACCMPilot.Flight.Control.Altitude.PositionPID
import           SMACCMPilot.Flight.Control.Altitude.ThrottleUI
import           SMACCMPilot.Flight.Types.MaybeFloat

import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleMode    as TM
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSetpoint as SP
import           SMACCMPilot.Comm.Ivory.Types.UserInput       ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlOutput as CO
import           SMACCMPilot.Comm.Ivory.Types.TimeMicros
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

data AltitudeControl =
   AltitudeControl
    { alt_init   :: forall eff   . Ivory eff ()
    , alt_update :: forall eff s1 s2 s3 s4
                 . Ref s1 ('Struct "sensors_result")
                -> Ref s2 ('Struct "user_input")
                -> Ref s3 ('Struct "control_setpoint")
                -> Ref s4 ('Struct "control_law")
                -> IFloat -- dt, seconds
                -> Ivory eff ()
    , alt_output :: forall eff s . Ref s ('Struct "control_output")
                              -> Ivory eff ()
    , alt_debug :: forall eff s . Ref s ('Struct "alt_control_debug")
                              -> Ivory eff ()
    }

monitorAltitudeControl :: (AttrReadable a)
                       => ControllableVehicleAttrs a
                       -> Monitor e AltitudeControl
monitorAltitudeControl attrs = do
  -- Alt estimator filters noisy sensor into altitude & its derivative
  alt_estimator    <- monitorAltEstimator
  -- Throttle tracker keeps track of steady-state throttle for transitioning
  -- into autothrottle mode
  throttle_tracker <- monitorThrottleTracker
  -- Thrust PID controls altitude rate with thrust
  thrust_pid       <- monitorThrustPid   (altitudeRatePid attrs) alt_estimator
  -- Position PID controls altitude with altitude rate
  position_pid     <- monitorPositionPid (altitudePositionPid attrs) alt_estimator
  -- UI controls Position setpoint from user stick input
  ui_control       <- monitorThrottleUI  (throttleUi attrs) alt_estimator
  -- setpoint: result of the whole autothrottle calculation
  at_setpt <- state "at_setpt"
  -- setpoint for manual passthrough control, and a boolean indicating
  -- whether autothrottle is valid or not
  ui_setpt <- state "ui_setpt"
  at_enabled <- state "at_enabled"

  monitorModuleDef $ do
    depend controlPIDModule -- for fconstrain
    depend maybeFloatModule
  -- state is saved in a struct, to be marshalled into a mavlink message
  state_dbg        <- state "state_debug"
  let named n = fmap showUnique $ freshname $ "alt_ctl_" ++ n
  name_alt_update <- named "update"
  name_alt_debug <- named "debug"
  name_alt_output <- named "output"

  let proc_alt_update :: Def('[ Ref s1 ('Struct "sensors_result")
                              , Ref s2 ('Struct "user_input")
                              , Ref s3 ('Struct "control_setpoint")
                              , Ref s4 ('Struct "control_law")
                              , IFloat
                              ]':->())
      proc_alt_update = proc name_alt_update $ \sens ui ctl_sp cl dt -> body $ do

          thr_mode <- deref (cl ~> CL.control_modes ~> CM.thr_mode)
          armed_mode <- deref (cl ~> CL.arming_mode)
          enabled <- assign ((armed_mode ==? A.armed)
                         .&& (thr_mode /=? TM.directUi))

          -- store ui_setpt =<< manual_throttle ui
          mt <- manual_throttle ui
          store ui_setpt mt
          store at_enabled enabled

          -- Update estimators
          tt_update throttle_tracker ui enabled

          sensor_alt  <- deref (sens ~> S.baro_alt)
          sensor_time <- deref (sens ~> S.baro_time)
          ae_measurement alt_estimator sensor_alt (timeMicrosToITime sensor_time)

          when enabled $ do
            vz_control <- cond
              -- default mode
              [ thr_mode ==? TM.altUi ==> do
                  -- update setpoint ui
                  tui_update      ui_control ui dt
                  -- ALTITUDE CONTROLLER START
                  -- update position controller (see stabilize_from_angle in Stabilize.hs)
                  -- we care only about the altitude, not the rate at this point
                  (ui_alt, _) <- tui_setpoint ui_control
                  alt_err     <- assign $ ui_alt - sensor_alt
                  -- ideally we would feed the controller just the desired position and the actual position
                  -- it should calculate alt_err internally
                  alt_rate_desired  <- call pid_update alt_pos_pid alt_pos_cfg alt_error sensor_alt
                  alt_rate_error    <- assign $ alt_rate_desired - sensor_alt_rate_est
                  -- again the error should be calculated internally
                  alt_thrust  <- call pid_update alt_rate_pid alt_rate_cfg alt_rate_error sensor_alt_rate_est
                  -- optionally limit the output of the feedback loop (like -0.2 -- 0.2), leave unlimited by default
                  alt_thrust_norm <- call fconstrain (-max_alt_thrust_fb)
                            max_alt_thrust_fb alt_thrust
                  -- ALTITUDE CONTROLLER END
                  -- add the constant hover throttle (~50% for now)
                  -- save this somewhere in the conf file
                  vz_ctl <- alt_thrust_norm + alt_nominal_hover_throttle     
                  -- maybe rename A.pos_rate_setp because it is not the right name             
                  store (state_dbg ~> A.pos_rate_setp) vz_ctl
                  return vz_ctl
              -- Ignore for now
              , thr_mode ==? TM.altSetpt ==> do
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
            -- Probably unused? This is the throttle_tracker_code
            -- Manage thrust pid integral reset, if required.
            -- (reset_integral, new_integral) <- tt_reset_to throttle_tracker
            -- when reset_integral $ do
            --  thrust_pid_set_integral thrust_pid new_integral
            
            -- calculate thrust pid (delete this)
            -- uncomp_thr_setpt <- thrust_pid_calculate thrust_pid vz_control dt
            
            -- compensate for roll/pitch rotation 
            r22              <- sensorsR22 sens
            setpt <- assign ((throttleR22Comp r22) * vz_ctl)
            -- limit max throttle by the throttle stick (safety feature)
            store at_setpt =<< call fconstrain 0.0 ui_setpt setpt
            
            -- now limit the throttle 

          unless enabled $ do
            tui_reset       ui_control
            -- Reset derivative tracking when not using thrust controller
            thrust_pid_init thrust_pid

      proc_alt_debug :: Def('[Ref s ('Struct "alt_control_debug")]':->())
      proc_alt_debug = proc name_alt_debug $ \out -> body $ do
        tui_write_debug ui_control state_dbg
        ae_write_debug alt_estimator state_dbg
        pos_pid_write_debug position_pid state_dbg
        thrust_pid_write_debug thrust_pid state_dbg
        refCopy out (constRef state_dbg)

      proc_alt_output :: Def('[Ref s ('Struct "control_output")]':->())
      proc_alt_output = proc name_alt_output $ \out -> body $ do
        at <- deref at_setpt
        en <- deref at_enabled
        ui <- deref ui_setpt
        ifte_ en
          (store (out ~> CO.throttle) at)
          (store (out ~> CO.throttle) ui)

  monitorModuleDef $ do
    incl proc_alt_update
    incl proc_alt_debug
    incl proc_alt_output
  return AltitudeControl
    { alt_init = do
        store at_enabled false
        ae_init alt_estimator
        tt_init throttle_tracker
        thrust_pid_init thrust_pid
    , alt_update = call_ proc_alt_update
    , alt_output = call_ proc_alt_output
    , alt_debug  = call_ proc_alt_debug
    }

-- | Calculate R22 factor, product of cosines of roll & pitch angles
sensorsR22 :: Ref s ('Struct "sensors_result") -> Ivory eff IFloat
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
  (r22 >? 0.8) ? (
    (1.0 / r22)
  , (r22 >? 0.0) ? (
      (((1.0 / 0.8 - 1.0) / 0.8) * r22 + 1.0)
    , (1.0)))


timeMicrosToITime :: TimeMicros -> ITime
timeMicrosToITime (TimeMicros m) = fromIMicroseconds m
