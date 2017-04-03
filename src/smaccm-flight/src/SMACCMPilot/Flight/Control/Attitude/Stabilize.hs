{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

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
  , IFloat                            -- stick_angle_rad
  , IFloat                            -- angle_measured_rad
  , IFloat                            -- rate_measured_rad_s
  , IFloat                            -- max_servo_rate_deg_s
  , IFloat                            -- dt [seconds]
  ] ':-> IFloat)
stabilize_from_angle = proc "stabilize_from_angle" $
  \angle_pid angle_cfg
   stick_angle_rad
   angle_measured_rad
   rate_measured_rad_s
   _max_servo_rate_deg_s
   dt ->
  requires (_max_servo_rate_deg_s /=? 0) $ body $
  do
  -- TODO: plug in the reference generators, we are using ref_accel and ref_rate = 0.0 (only regulating rate and accel)  
  stabilize_cmd <- call pid_update angle_pid angle_cfg stick_angle_rad angle_measured_rad 0.0 rate_measured_rad_s 0.0 dt

  -- for now limit to +-100% (of throttle) and then normalize to 0-1 range
  -- what about rounding errors with too small gains? (ideally we have everything scaled to [-1,1]
  stabilize_cmd_norm   <- call fconstrain (-1.0) 1.0 stabilize_cmd
  ret $ stabilize_cmd_norm -- / 1.0


-- | Return a normalized servo output given a normalized stick input
-- representing the desired rate.  Only uses the rate PID controller.
stabilize_from_rate :: Def (
 '[ Ref      s1 ('Struct "pid_state")     -- rate_pid
  , ConstRef s2 ('Struct "pid_config")    -- rate_cfg
  , IFloat                              -- stick_rate_rad_s
  , IFloat                              -- rate_measured_rad_s
  , IFloat                              -- max_servo_rate_deg_s
  , IFloat                              -- dt [seconds]
  ] ':-> IFloat)
stabilize_from_rate = proc "stabilize_from_rate" $
  \rate_pid rate_cfg stick_rate_rad_s
   rate_measured_rad_s max_servo_rate_deg_s dt ->
  requires (max_servo_rate_deg_s /=? 0) $ body $
  do
{-
  last_rate <- 0.0
  d_rate <- 0.0
  store d_rate (rate_measured_rad_s - last_rate)
  store last_rate d_rate
-}
  -- TODO: measure rate of change?
  rate_cmd  <- call pid_update rate_pid rate_cfg stick_rate_rad_s rate_measured_rad_s 0.0 0.0 0.0 dt
  rate_cmd_norm   <- call fconstrain (-1.0) 1.0 rate_cmd
  ret $ rate_cmd_norm -- / 1.0
