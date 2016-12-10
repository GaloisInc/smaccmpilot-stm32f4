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

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker
import           SMACCMPilot.Flight.Control.Altitude.ThrottleUI
import           SMACCMPilot.Flight.Types.MaybeFloat
import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleMode    as TM
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
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
  alt_estimator <- monitorAltEstimator
  -- Thrust PID controls altitude rate with thrust
  alt_rate_pid <- state "alt_rate_pid"
  -- Position PID controls altitude with altitude rate
  alt_pos_pid <- state "alt_pos_pid"
  alt_pos_cfg <- attrState (altitudePositionPid attrs)
  _alt_nominal_throttle <- attrState (nominalThrottle attrs)
  -- UI controls Position setpoint from user stick input
  ui_control <- monitorThrottleUI  (throttleUi attrs) alt_estimator
  -- setpoint: result of the whole autothrottle calculation
  at_setpt <- state "at_setpt"
  -- setpoint for manual passthrough control, and a boolean indicating
  -- whether autothrottle is valid or not
  ui_setpt <- state "ui_setpt"
  at_enabled <- state "at_enabled"

  nominal_throttle <- attrState (nominalThrottle attrs)

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
      proc_alt_update =
        proc name_alt_update $ \sens ui _ctl_sp cl dt -> body $ do
          thr_mode <- deref (cl ~> CL.control_modes ~> CM.thr_mode)
          armed_mode <- deref (cl ~> CL.arming_mode)
          enabled <- assign ((armed_mode ==? A.armed)
                         .&& (thr_mode /=? TM.directUi))

          mt <- manual_throttle ui
          store ui_setpt mt
          store at_enabled enabled

          -- Update estimators
          alt_lidar_alt  <- deref (sens ~> S.baro_alt)
          alt_lidar_time <- deref (sens ~> S.baro_time)
          ae_measurement
            alt_estimator
            alt_lidar_alt
            (timeMicrosToITime alt_lidar_time)

          -- read newest estimate
          (alt_est_pos, alt_est_rate) <- ae_state alt_estimator

          when enabled $ do
            vz_control <- cond
              -- default mode
              [thr_mode ==? TM.altUi ==> do
                  -- update setpoint ui
                  tui_update      ui_control ui dt
                  (ui_alt, _) <- tui_setpoint ui_control
                  -- store errors for reference
                  _alt_err     <- assign $ ui_alt - alt_est_pos
                  _alt_rate_err    <- assign $ 0.0 - alt_est_rate
                  store (state_dbg ~> A.pos_setp) _alt_err
                  store (state_dbg ~> A.pos_rate_setp) _alt_rate_err

                  -- ALTITUDE CONTROLLER START (SIMPLE PID with
                  -- constant nominal thrust) update position
                  -- controller (see stabilize_from_angle iMonitor n
                  -- Stabilize.hs) we care only about the altitude,
                  -- not the rate at this point
                  --
                  -- PID: zero rate for now
                  thrust_cmd <-
                    call pid_update
                      alt_pos_pid
                      (constRef alt_pos_cfg)
                      ui_alt
                      alt_est_pos
                      0.0
                      alt_est_rate
                      0.0

                  -- thrust_cmd is unbounded, so make sure it is
                  -- within the limits [0.1-1]
                  thrust_cmd_norm   <- call fconstrain (0.1) 1 thrust_cmd

                  -- compensate for roll/pitch rotation
                  r22   <- _sensorsR22 sens
                  hover_thrust_abs <- deref nominal_throttle
                  hover_thrust <-
                    assign ((_throttleR22Comp r22) * hover_thrust_abs)
                  let vz_ctl = thrust_cmd_norm + hover_thrust
                  -- ALTITUDE CONTROLLER END

                  -- maybe rename A.pos_rate_setp because it is not
                  -- the right name
                  store (state_dbg ~> A.pos_rate_setp) vz_ctl
                  return vz_ctl
              ]

            setpt <- assign vz_control

            -- limit max throttle by the throttle stick (safety feature)
            store at_setpt =<< call fconstrain 0.0 mt setpt

          ifte_ enabled
            (do sp <- deref at_setpt
                store (state_dbg ~> A.pos_rate_setp) sp
            )
            (do ui_sp <- deref ui_setpt
                store (state_dbg ~> A.pos_rate_setp) ui_sp
            )

          unless enabled $ do
            tui_reset       ui_control
            -- reset the integrators in manual mode
            call_ pid_reset alt_pos_pid
            call_ pid_reset alt_rate_pid

      proc_alt_debug :: Def('[Ref s ('Struct "alt_control_debug")]':->())
      proc_alt_debug = proc name_alt_debug $ \out -> body $ do
        tui_write_debug ui_control state_dbg
        ae_write_debug alt_estimator state_dbg
        --pos_pid_write_debug position_pid state_dbg
        --thrust_pid_write_debug thrust_pid state_dbg
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
        -- tt_init throttle_tracker
        -- thrust_pid_init thrust_pid
    , alt_update = call_ proc_alt_update
    , alt_output = call_ proc_alt_output
    , alt_debug  = call_ proc_alt_debug
    }

-- | Calculate R22 factor, product of cosines of roll & pitch angles
_sensorsR22 :: Ref s ('Struct "sensors_result") -> Ivory eff IFloat
_sensorsR22 sens = do
  pitch <- deref (sens ~> S.pitch)
  roll  <- deref (sens ~> S.roll)
  r22   <- assign (cos pitch * cos roll)
  return r22

-- | Throttle compensation factor (multiplicative) based on R22
--   Based on PX4 project code (98a43454 Anton Babushkin)
--   R22 is rotation matrix element {2,2}, equivelant to cos pitch * cos roll
_throttleR22Comp :: IFloat -> IFloat
_throttleR22Comp r22 =
  (r22 >? 0.8) ? (
    (1.0 / r22)
  , (r22 >? 0.0) ? (
      (((1.0 / 0.8 - 1.0) / 0.8) * r22 + 1.0)
    , (1.0)))


timeMicrosToITime :: TimeMicros -> ITime
timeMicrosToITime (TimeMicros m) = fromIMicroseconds m
