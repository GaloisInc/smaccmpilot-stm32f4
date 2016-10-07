{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.Stabilize
  ( stabilize_from_angle
  , stabilize_from_rate
  , attStabilizeModule
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Control.PID

attStabilizeModule :: Module
attStabilizeModule = package "attitude_stabilize" $ do
  depend controlPIDModule
  incl stabilize_from_angle
  incl stabilize_from_rate

-- | Return a normalized servo output given a normalized stick input
-- representing the desired angle.  This uses two PI controllers; one
-- to convert the desired angle to a rate of rotation, and the other
-- to control the rate.
--
-- See the section "How PIs work" at this wiki page for details:
--
--   http://code.google.com/p/arducopter/wiki/AC2_Tweaks
stabilize_from_angle :: Def (
 '[ Ref      s1 ('Struct "pid_state")   -- angle_pid
  , ConstRef s2 ('Struct "pid_config")  -- angle_cfg
  , Ref      s3 ('Struct "pid_state")   -- rate_pid
  , ConstRef s4 ('Struct "pid_config")  -- rate_cfg
  , IFloat                            -- stick_angle_rad
  , IFloat                            -- angle_measured_rad
  , IFloat                            -- rate_measured_rad_s
  , IFloat                            -- max_servo_rate_deg_s
  ] ':-> IFloat)
stabilize_from_angle = proc "stabilize_from_angle" $
  \angle_pid angle_cfg
   rate_pid  rate_cfg
   stick_angle_rad
   angle_measured_rad
   rate_measured_rad_s
   _max_servo_rate_deg_s ->
  requires (_max_servo_rate_deg_s /=? 0) $ body $
  do
  -- TODO: plug in the reference generators, we are using ref_accel and ref_rate = 0.0 (only regulating rate and accel)  
  stabilize_cmd <- call pid_update angle_pid angle_cfg stick_angle_rad angle_measured_rad 0.0 rate_measured_rad_s 0.0

  -- for now limit to +-100% (of throttle) and then normalize to 0-1 range
  stabilize_cmd_norm   <- call fconstrain (-100.0)
                            100.0 stabilize_cmd
  ret $ stabilize_cmd_norm / 100.0


-- | Return a normalized servo output given a normalized stick input
-- representing the desired rate.  Only uses the rate PID controller.
stabilize_from_rate :: Def (
 '[ Ref      s1 ('Struct "pid_state")     -- rate_pid
  , ConstRef s2 ('Struct "pid_config")    -- rate_cfg
  , IFloat                              -- stick_rate_rad_s
  , IFloat                              -- rate_measured_rad_s
  , IFloat                              -- max_servo_rate_deg_s
  ] ':-> IFloat)
stabilize_from_rate = proc "stabilize_from_rate" $
  \rate_pid rate_cfg stick_rate_rad_s
   rate_measured_rad_s max_servo_rate_deg_s ->
  requires (max_servo_rate_deg_s /=? 0) $ body $
  do
  servo_rate_deg_s  <- call pid_update rate_pid rate_cfg stick_rate_rad_s rate_measured_rad_s 
  servo_rate_norm   <- call fconstrain (-max_servo_rate_deg_s)
                            max_servo_rate_deg_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_deg_s


----------------------------------------------------------------------
-- Math Utilities (move into a Math.hs?)

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105

