{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Stabilize where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Param

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
  depend controlPIDModule
  defMemArea pid_roll_stabilize
  defMemArea pid_roll_rate
  defMemArea pid_pitch_stabilize
  defMemArea pid_pitch_rate
  defMemArea pid_yaw_rate
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

-- | Read a set of PID parameters into a PIDConfig structure.
getPIDParams :: (GetAlloc eff ~ Scope s2)
             => PIDParams ParamReader
             -> Ref s1 (Struct "PIDConfig")
             -> Ivory eff ()
getPIDParams p ref = do
  storeParam     pid_pGain pidP
  storeParam     pid_iGain pidI
  storeParam     pid_dGain pidD
  storeParam     pid_iMax  pidImax
  storeParamWith pid_iMin  pidImax negate
  where
    storeParamWith slot accessor f = do
      x <- paramRead (accessor p)
      store (ref ~> slot) (f (paramData x))
    storeParam s a = storeParamWith s a id

allocPIDParams :: (GetAlloc eff ~ Scope s)
               => PIDParams ParamReader
               -> Ivory eff (Ref (Stack s) (Struct "PIDConfig"))
allocPIDParams p = do
  cfg <- local izero
  getPIDParams p cfg
  return cfg

makeStabilizeRun :: FlightParams ParamReader
                 -> Def ('[ ConstRef s0 (Struct "flightmode")
                          , ConstRef s1 (Struct "userinput_result")
                          , ConstRef s2 (Struct "sensors_result")
                          , Ref s3 (Struct "controloutput")
                          ] :-> ())
makeStabilizeRun params = proc "stabilize_run" $ \fm input sensors output -> body $ do
  armed <- (fm ~>* FM.armed)
  ifte_ (iNot armed)
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

      -- Initialize PID configuration from parameters.
      roll_stabilize_cfg  <- allocPIDParams (flightRollStab  params)
      roll_rate_cfg       <- allocPIDParams (flightRollRate  params)
      pitch_stabilize_cfg <- allocPIDParams (flightPitchStab params)
      pitch_rate_cfg      <- allocPIDParams (flightPitchRate params)
      yaw_rate_cfg        <- allocPIDParams (flightYawRate   params)

      roll_out <- call stabilize_from_angle
                          roll_stabilize
                          (constRef roll_stabilize_cfg)
                          roll_rate
                          (constRef roll_rate_cfg)
                          const_MAX_INPUT_ROLL
                          controlinput_roll
                          sen_roll
                          sen_omega_x
                          const_MAX_OUTPUT_ROLL

      pitch_out <- call stabilize_from_angle
                          pitch_stabilize
                          (constRef pitch_stabilize_cfg)
                          pitch_rate
                          (constRef pitch_rate_cfg)
                          const_MAX_INPUT_ROLL
                          (-1 * controlinput_pitch)
                          sen_pitch
                          sen_omega_y
                          const_MAX_OUTPUT_ROLL

      yaw_out <- call stabilize_from_rate
                          yaw_rate
                          (constRef yaw_rate_cfg)
                          controlinput_yaw
                          const_MAX_INPUT_YAW
                          sen_omega_z
                          const_MAX_OUTPUT_YAW

      store (output ~> OUT.roll)  roll_out
      store (output ~> OUT.pitch) pitch_out
      store (output ~> OUT.yaw)   yaw_out
  where
  do_reset :: Ref s (Struct "PIDState") -> Ivory eff ()
  do_reset pid = store (pid ~> pid_reset) 1
  roll_stabilize  = addrOf pid_roll_stabilize
  pitch_stabilize = addrOf pid_pitch_stabilize
  roll_rate       = addrOf pid_roll_rate
  pitch_rate      = addrOf pid_pitch_rate
  yaw_rate        = addrOf pid_yaw_rate

----------------------------------------------------------------------
-- PID initializers

pid_roll_stabilize :: MemArea (Struct "PIDState")
pid_roll_stabilize = area "g_pid_roll_stabilize" $ Just $ istruct
  [ pid_reset .= ival 1 ]

pid_roll_rate :: MemArea (Struct "PIDState")
pid_roll_rate = area "g_pid_roll_rate" $ Just $ istruct
  [ pid_reset .= ival 1 ]

pid_pitch_stabilize :: MemArea (Struct "PIDState")
pid_pitch_stabilize = area "g_pid_pitch_stabilize" $ Just $ istruct
  [ pid_reset .= ival 1 ]

pid_pitch_rate :: MemArea (Struct "PIDState")
pid_pitch_rate = area "g_pid_pitch_rate" $ Just $ istruct
  [ pid_reset .= ival 1 ]

pid_yaw_rate :: MemArea (Struct "PIDState")
pid_yaw_rate = area "g_pid_yaw_rate" $ Just $ istruct
  [ pid_reset .= ival 1 ]

-- | Initialize the stabilization module.  This registers parameters
-- for all the PID controllers so they can be accessed via MAVlink.
stabilize_init :: Def ('[] :-> ())
stabilize_init = proc "stabilize_init" $ body $ do
  retVoid

-- | Return a normalized servo output given a normalized stick input
-- representing the desired angle.  This uses two PI controllers; one
-- to convert the desired angle to a rate of rotation, and the other
-- to control the rate.
--
-- See the section "How PIs work" at this wiki page for details:
--
--   http://code.google.com/p/arducopter/wiki/AC2_Tweaks
stabilize_from_angle :: Def (
 '[ Ref      s1 (Struct "PIDState")   -- angle_pid
  , ConstRef s2 (Struct "PIDConfig")  -- angle_cfg
  , Ref      s3 (Struct "PIDState")   -- rate_pid
  , ConstRef s4 (Struct "PIDConfig")  -- rate_cfg
  , IFloat                            -- stick_angle_norm
  , IFloat                            -- max_stick_angle_deg
  , IFloat                            -- sensor_angle_rad
  , IFloat                            -- sensor_rate_rad_s
  , IFloat                            -- max_servo_rate_rad_s
  ] :-> IFloat)
stabilize_from_angle = proc "stabilize_from_angle" $
  \angle_pid angle_cfg
   rate_pid  rate_cfg
   stick_angle_norm max_stick_angle_deg
   sensor_angle_rad sensor_rate_rad_s
   max_servo_rate_rad_s ->
  requires (max_servo_rate_rad_s /=? 0) $ body $
  do
  stick_angle_deg   <- assign $ stick_angle_norm * max_stick_angle_deg
  sensor_angle_deg  <- assign $ degrees sensor_angle_rad
  angle_error       <- assign $ stick_angle_deg - sensor_angle_deg
  rate_deg_s        <- call pid_update angle_pid angle_cfg angle_error sensor_angle_deg
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_cfg rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_rad_s

-- | Return a normalized servo output given a normalized stick input
-- representing the desired rate.  Only uses the rate PID controller.
stabilize_from_rate :: Def (
 '[ Ref      s1 (Struct "PIDState")     -- rate_pid
  , ConstRef s2 (Struct "PIDConfig")    -- rate_cfg
  , IFloat                              -- stick_rate_norm
  , IFloat                              -- max_stick_rate_deg_s
  , IFloat                              -- sensor_rate_rad_s
  , IFloat                              -- max_servo_rate_rad_s
  ] :-> IFloat)
stabilize_from_rate = proc "stabilize_from_rate" $
  \rate_pid rate_cfg stick_rate_norm max_stick_rate_deg_s
   sensor_rate_rad_s max_servo_rate_rad_s ->
  requires (max_servo_rate_rad_s /=? 0) $ body $
  do
  stick_rate_deg_s  <- assign $ stick_rate_norm * max_stick_rate_deg_s
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ stick_rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_cfg rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_rad_s


----------------------------------------------------------------------
-- Math Utilities (move into a Math.hs?)

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105

