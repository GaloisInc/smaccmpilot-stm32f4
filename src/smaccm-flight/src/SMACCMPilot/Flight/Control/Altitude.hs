{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Altitude controller based off work by Anton Babushkin <rk3dov@gmail.com> for
-- the PX4 Autopilot Project
-- https://github.com/PX4/Firmware/tree/master/src/modules/multirotor_pos_control

module SMACCMPilot.Flight.Control.Altitude
  ( AltitudeControl(..)
  , monitorAltitudeControl
  ) where

import Prelude ()
import Prelude.Compat

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import Linear
import Numeric.Estimator.Model.Coordinate

import           SMACCMPilot.Flight.Control.Altitude.KalmanFilter
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker
import           SMACCMPilot.Flight.Control.Altitude.ThrottleUI
import           SMACCMPilot.Flight.Types.MaybeFloat
import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleMode    as TM
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.Quaternion      as Q
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz             as XYZ
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
  r22_dbg <- state "r22_dbg"
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
          -- TODO: more sophisticated noise values
          r22 <- sensorsR22 sens
          store r22_dbg r22
          att_quat <- derefQuat (sens ~> S.attitude)
          accel <- derefXyz (sens ~> S.accel)
          let NED (V3 _ _ zdot) = fst (convertFrames att_quat) accel
          ae_propagate alt_estimator zdot dt

          -- baro: offset, no compensation
          alt_baro_alt  <- deref (sens ~> S.baro_alt)
          ae_measure_offset alt_estimator alt_baro_alt r_baro

          -- lidar and sonar: no offset, compensated
          let gain = (abs r22 >? 0) ? (abs r22, 1e-7)
          alt_lidar_alt <- deref (sens ~> S.lidar_alt)
          -- if outside of effective range, don't measure (see
          -- lidarlite datasheet)
          unless (0 >? alt_lidar_alt .|| alt_lidar_alt >? 40) $ do
            -- adjust noise based on the ranges given in the datasheet
            noise <- ifte (alt_lidar_alt <? 5) (pure 0.025) (pure 0.1)
            ae_measure_absolute alt_estimator (alt_lidar_alt * gain) noise
          alt_sonar_alt <- deref (sens ~> S.sonar_alt)
          ae_measure_absolute alt_estimator (alt_sonar_alt * gain) r_alt

          -- read newest estimate
          (AltState{..}) <- ae_state alt_estimator

          when enabled $ do
            vz_control <- cond
              -- default mode
              [thr_mode ==? TM.altUi ==> do
                  -- update setpoint ui
                  tui_update      ui_control ui dt
                  (ui_alt, _) <- tui_setpoint ui_control
                  -- store errors for reference
                  _alt_err     <- assign $ ui_alt - as_z
                  _alt_rate_err    <- assign $ 0.0 - as_zdot
                  store (state_dbg ~> A.pos_err) _alt_err
                  store (state_dbg ~> A.pos_rate_err) _alt_rate_err

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
                      as_z
                      0.0
                      as_zdot
                      0.0
                      dt

                  -- thrust_cmd is unbounded, so make sure it is
                  -- within the limits [0.1-1]
                  thrust_cmd_norm   <- call fconstrain (0.1) 0.9 thrust_cmd

                  -- compensate for roll/pitch rotation
                  hover_thrust_abs <- deref nominal_throttle
                  hover_thrust <-
                    assign ((throttleR22Comp r22) * hover_thrust_abs)
                  let vz_ctl = thrust_cmd_norm + hover_thrust
                  -- ALTITUDE CONTROLLER END

                  -- maybe rename A.pos_rate_setp because it is not
                  -- the right name
                  --store (state_dbg ~> A.vz_ctl) vz_ctl
                  return vz_ctl
              ]

            setpt <- assign vz_control

            -- limit max throttle by the throttle stick (safety feature)
            store at_setpt =<< call fconstrain 0.0 mt setpt

          ifte_ enabled
            (do sp <- deref at_setpt
                store (state_dbg ~> A.vz_ctl) sp
            )
            (do ui_sp <- deref ui_setpt
                store (state_dbg ~> A.vz_ctl) ui_sp
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
        gain <- abs <$> deref r22_dbg
        store (state_dbg ~> A.r22_gain) gain
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

derefXyz :: Ref s ('Struct "xyz") -> Ivory eff (XYZ IFloat)
derefXyz r = fmap XYZ $ mapM deref $ fmap (r ~>) (V3 XYZ.x XYZ.y XYZ.z)

derefQuat :: Ref s ('Struct "quaternion") -> Ivory eff (Quaternion IFloat)
derefQuat quat = do
  a <- deref (quat ~> Q.quat_a)
  b <- deref (quat ~> Q.quat_b)
  c <- deref (quat ~> Q.quat_c)
  d <- deref (quat ~> Q.quat_d)
  return (Quaternion a (V3 b c d))

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

_timeMicrosToITime :: TimeMicros -> ITime
_timeMicrosToITime (TimeMicros m) = fromIMicroseconds m
