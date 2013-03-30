{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.Control.Stabilize where

import Ivory.Language
import SMACCMPilot.Util.IvoryHelpers

import SMACCMPilot.Param

import SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Flight.Types.FlightMode as FM
import qualified SMACCMPilot.Flight.Types.UserInput as IN
import qualified SMACCMPilot.Flight.Types.Sensors as SEN
import qualified SMACCMPilot.Flight.Types.ControlOutput as OUT

----------------------------------------------------------------------
-- Ivory Module

stabilizeControlLoopsModule :: Module
stabilizeControlLoopsModule = package "stabilize_controlloops" $ do
  depend FM.flightModeTypeModule
  depend IN.userInputTypeModule
  depend SEN.sensorsTypeModule
  depend OUT.controlOutputTypeModule
  depend paramModule
  depend controlPIDModule
  defMemArea pid_roll_stabilize
  defMemArea pid_roll_rate
  defMemArea pid_pitch_stabilize
  defMemArea pid_pitch_rate
  defMemArea pid_yaw_rate
  incl stabilize_run
  incl stabilize_init
  incl stabilize_from_angle
  incl stabilize_from_rate

----------------------------------------------------------------------
-- Stabilization

const_MAX_INPUT_ROLL, const_MAX_INPUT_PITCH, const_MAX_INPUT_YAW :: IFloat
const_MAX_INPUT_ROLL  = 45  -- deg
const_MAX_INPUT_PITCH = 45  -- deg
const_MAX_INPUT_YAW   = 180 -- deg/sec

const_MAX_OUTPUT_ROLL, const_MAX_OUTPUT_PITCH, const_MAX_OUTPUT_YAW :: IFloat
const_MAX_OUTPUT_ROLL  = 50 -- deg/sec
const_MAX_OUTPUT_PITCH = 50 -- deg/sec
const_MAX_OUTPUT_YAW   = 45 -- deg/sec


stabilize_run :: Def ('[ Ref s0 (Struct "flightmode")
                       , Ref s1 (Struct "userinput_result")
                       , Ref s2 (Struct "sensors_result")
                       , Ref s3 (Struct "controloutput")
                       ] :-> ())
stabilize_run = proc "stabilize_run" $ \fm input sensors output -> body $ do
  roll_stabilize  <- addrOf pid_roll_stabilize
  pitch_stabilize <- addrOf pid_pitch_stabilize
  roll_rate       <- addrOf pid_roll_rate
  pitch_rate      <- addrOf pid_pitch_rate
  yaw_rate        <- addrOf pid_yaw_rate

  armed <- (fm ~>* FM.armed)
  ifte (iNot armed)
    (mapM_ do_reset
      [roll_stabilize, pitch_stabilize, roll_rate, pitch_rate, yaw_rate] )
    $ do -- if armed:
      sen_roll    <- (sensors ~>* SEN.roll)
      sen_pitch   <- (sensors ~>* SEN.pitch)
      sen_omega_x <- (sensors ~>* SEN.omega_x)
      sen_omega_y <- (sensors ~>* SEN.omega_y)
      sen_omega_z <- (sensors ~>* SEN.omega_z)

      controlinput_roll  <- (input ~>* IN.roll)
      controlinput_pitch <- (input ~>* IN.pitch)
      controlinput_yaw   <- (input ~>* IN.yaw)

      roll_out <- call (direct stabilize_from_angle
                          roll_stabilize
                          roll_rate
                          const_MAX_INPUT_ROLL
                          controlinput_roll
                          sen_roll
                          sen_omega_x
                          const_MAX_OUTPUT_ROLL)

      pitch_out <- call (direct stabilize_from_angle
                          pitch_stabilize
                          pitch_rate
                          const_MAX_INPUT_ROLL
                          (-1 * controlinput_pitch)
                          sen_pitch
                          sen_omega_y
                          const_MAX_OUTPUT_ROLL)

      yaw_out <- call (direct stabilize_from_rate
                          yaw_rate
                          controlinput_yaw
                          const_MAX_INPUT_YAW
                          sen_omega_z
                          const_MAX_OUTPUT_YAW)

      store (output ~> OUT.roll)  roll_out
      store (output ~> OUT.pitch) pitch_out
      store (output ~> OUT.yaw)   yaw_out
  where
  do_reset :: Ref s (Struct "PID") -> Ivory eff ()
  do_reset pid = store (pid ~> pid_reset) 1
----------------------------------------------------------------------
-- PID initializers

pid_roll_stabilize :: MemArea (Struct "PID")
pid_roll_stabilize = area "g_pid_roll_stabilize" $ Just $ istruct
  [ pid_pGain    .= ival 2.0
  , pid_iGain    .= ival 0.0
  , pid_dGain    .= ival 0.0
  , pid_iMin     .= ival (-8.0)
  , pid_iMax     .= ival 8.0
  , pid_reset    .= ival 1
  ]

pid_roll_rate :: MemArea (Struct "PID")
pid_roll_rate = area "g_pid_roll_rate" $ Just $ istruct
  [ pid_pGain    .= ival 0.15
  , pid_iGain    .= ival 0.015
  , pid_dGain    .= ival 0.0
  , pid_iMin     .= ival (-5.0)
  , pid_iMax     .= ival 5.0
  , pid_reset    .= ival 1
  ]

pid_pitch_stabilize :: MemArea (Struct "PID")
pid_pitch_stabilize = area "g_pid_pitch_stabilize" $ Just $ istruct
  [ pid_pGain    .= ival 2.0
  , pid_iGain    .= ival 0.0
  , pid_dGain    .= ival 0.0
  , pid_iMin     .= ival (-8.0)
  , pid_iMax     .= ival 8.0
  , pid_reset    .= ival 1
  ]

pid_pitch_rate :: MemArea (Struct "PID")
pid_pitch_rate = area "g_pid_pitch_rate" $ Just $ istruct
  [ pid_pGain    .= ival 0.15
  , pid_iGain    .= ival 0.015
  , pid_dGain    .= ival 0.0
  , pid_iMin     .= ival (-5.0)
  , pid_iMax     .= ival 5.0
  , pid_reset    .= ival 1
  ]

pid_yaw_rate :: MemArea (Struct "PID")
pid_yaw_rate = area "g_pid_yaw_rate" $ Just $ istruct
  [ pid_pGain    .= ival 0.3
  , pid_iGain    .= ival 0.015
  , pid_dGain    .= ival 0.0
  , pid_iMin     .= ival (-8.0)
  , pid_iMax     .= ival 8.0
  , pid_reset    .= ival 1
  ]

-- | Initialize the stabilization module.  This registers parameters
-- for all the PID controllers so they can be accessed via MAVlink.
stabilize_init :: Def ('[] :-> ())
stabilize_init = proc "stabilize_init" $ body $ do
  param_init_area "STB_RLL"  pid_roll_stabilize
  param_init_area "RATE_RLL" pid_roll_rate
  param_init_area "STB_PIT"  pid_pitch_stabilize
  param_init_area "RATE_PIT" pid_pitch_rate
  param_init_area "RATE_YAW" pid_yaw_rate

-- | Return a normalized servo output given a normalized stick input
-- representing the desired angle.  This uses two PI controllers; one
-- to convert the desired angle to a rate of rotation, and the other
-- to control the rate.
--
-- See the section "How PIs work" at this wiki page for details:
--
--   http://code.google.com/p/arducopter/wiki/AC2_Tweaks
stabilize_from_angle :: Def ('[
  (Ref s1 (Struct "PID")),      -- angle_pid
  (Ref s2 (Struct "PID")),      -- rate_pid
  IFloat,                       -- stick_angle_norm
  IFloat,                       -- max_stick_angle_deg
  IFloat,                       -- sensor_angle_rad,
  IFloat,                       -- sensor_rate_rad_s
  IFloat                        -- max_servo_rate_rad_s
 ] :-> IFloat)
stabilize_from_angle = proc "stabilize_from_angle" $
  \angle_pid rate_pid stick_angle_norm
   max_stick_angle_deg sensor_angle_rad
   sensor_rate_rad_s max_servo_rate_rad_s ->
  requires [check $ max_servo_rate_rad_s /=? 0] $ body $ 
  do
  stick_angle_deg   <- assign $ stick_angle_norm * max_stick_angle_deg
  sensor_angle_deg  <- assign $ degrees sensor_angle_rad
  angle_error       <- assign $ stick_angle_deg - sensor_angle_deg
  rate_deg_s        <- call (direct pid_update angle_pid angle_error sensor_angle_deg)
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call (direct pid_update rate_pid rate_error sensor_rate_deg_s)
  servo_rate_norm   <- call (direct fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s)
  ret $ servo_rate_norm / max_servo_rate_rad_s

-- | Return a normalized servo output given a normalized stick input
-- representing the desired rate.  Only uses the rate PID controller.
stabilize_from_rate :: Def ('[
  (Ref s1 (Struct "PID")),         -- rate_pid
  IFloat,                       -- stick_rate_norm
  IFloat,                       -- max_stick_rate_deg_s
  IFloat,                       -- sensor_rate_rad_s
  IFloat                        -- max_servo_rate_rad_s
 ] :-> IFloat)
stabilize_from_rate = proc "stabilize_from_rate" $
  \rate_pid stick_rate_norm max_stick_rate_deg_s
   sensor_rate_rad_s max_servo_rate_rad_s ->
  requires [check $ max_servo_rate_rad_s /=? 0] $ body $
  do
  stick_rate_deg_s  <- assign $ stick_rate_norm * max_stick_rate_deg_s
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ stick_rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call (direct pid_update rate_pid rate_error sensor_rate_deg_s)
  servo_rate_norm   <- call (direct fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s)
  ret $ servo_rate_norm / max_servo_rate_rad_s


----------------------------------------------------------------------
-- Math Utilities (move into a Math.hs?)

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105

