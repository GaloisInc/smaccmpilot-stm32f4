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

import SMACCMPilot.Param ()
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
 '[ Ref      s1 (Struct "PIDState")   -- angle_pid
  , ConstRef s2 (Struct "PIDConfig")  -- angle_cfg
  , Ref      s3 (Struct "PIDState")   -- rate_pid
  , ConstRef s4 (Struct "PIDConfig")  -- rate_cfg
  , IFloat                            -- stick_angle_norm
  , IFloat                            -- max_stick_angle_deg
  , IFloat                            -- sensor_angle_rad
  , IFloat                            -- sensor_rate_rad_s
  , IFloat                            -- max_servo_rate_deg_s
  ] :-> IFloat)
stabilize_from_angle = proc "stabilize_from_angle" $
  \angle_pid angle_cfg
   rate_pid  rate_cfg
   stick_angle_norm max_stick_angle_deg
   sensor_angle_rad sensor_rate_rad_s
   max_servo_rate_deg_s ->
  requires (max_servo_rate_deg_s /=? 0) $ body $
  do
  stick_angle_deg   <- assign $ stick_angle_norm * max_stick_angle_deg
  sensor_angle_deg  <- assign $ degrees sensor_angle_rad
  angle_error       <- assign $ stick_angle_deg - sensor_angle_deg
  rate_deg_s        <- call pid_update angle_pid angle_cfg angle_error sensor_angle_deg
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_cfg rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_deg_s)
                            max_servo_rate_deg_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_deg_s


-- | Return a normalized servo output given a normalized stick input
-- representing the desired rate.  Only uses the rate PID controller.
stabilize_from_rate :: Def (
 '[ Ref      s1 (Struct "PIDState")     -- rate_pid
  , ConstRef s2 (Struct "PIDConfig")    -- rate_cfg
  , IFloat                              -- stick_rate_norm
  , IFloat                              -- max_stick_rate_deg_s
  , IFloat                              -- sensor_rate_rad_s
  , IFloat                              -- max_servo_rate_deg_s
  ] :-> IFloat)
stabilize_from_rate = proc "stabilize_from_rate" $
  \rate_pid rate_cfg stick_rate_norm max_stick_rate_deg_s
   sensor_rate_rad_s max_servo_rate_deg_s ->
  requires (max_servo_rate_deg_s /=? 0) $ body $
  do
  stick_rate_deg_s  <- assign $ stick_rate_norm * max_stick_rate_deg_s
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ stick_rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_cfg rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_deg_s)
                            max_servo_rate_deg_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_deg_s


----------------------------------------------------------------------
-- Math Utilities (move into a Math.hs?)

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105

