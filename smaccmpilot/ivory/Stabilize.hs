{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- Stabilize.hs --- Pitch and roll stabilization PID controller.
--
-- Copyright (C) 2012, Galois, Inc.
-- All Rights Reserved.
--
-- TBD: License terms?
--

module Stabilize where

import Control.Applicative
import Data.String
import Ivory.Language

import IvoryHelpers
import SMACCMPilot.Param

----------------------------------------------------------------------
-- Ivory Module

stabilizeModule :: Module
stabilizeModule = package "pid_stabilize" $ do
  depend paramModule
  defStruct (Proxy :: Proxy "PID")
  defMemArea pid_roll_stabilize
  defMemArea pid_roll_rate
  defMemArea pid_pitch_stabilize
  defMemArea pid_pitch_rate
  defMemArea pid_yaw_rate
  incl fconstrain
  incl pid_update
  incl stabilize_init
  incl stabilize_from_angle
  incl stabilize_from_rate

----------------------------------------------------------------------
-- Math Utilities (move into a Math.hs?)

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] :-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte (x <? xmin)
    (ret xmin)
    (ifte (x >? xmax)
      (ret xmax)
      (ret x)))

----------------------------------------------------------------------
-- Generic PID Controller

[ivory|
  struct PID
    { pid_pGain  :: Stored IFloat
    ; pid_iGain  :: Stored IFloat
    ; pid_dGain  :: Stored IFloat
    ; pid_iState :: Stored IFloat
    ; pid_iMin   :: Stored IFloat
    ; pid_iMax   :: Stored IFloat
    ; pid_dState :: Stored IFloat
    ; pid_reset  :: Stored Uint8
  }
|]

-- | Update a PID controller given an error value and measured value
-- and return the output value.
pid_update :: Def ('[(Ref s1 (Struct "PID")), IFloat, IFloat] :-> IFloat)
pid_update = proc "pid_update" $ \pid err pos -> body $ do
  p_term  <- fmap (* err) (pid~>*pid_pGain)

  i_min   <- pid~>*pid_iMin
  i_max   <- pid~>*pid_iMax
  pid~>pid_iState %=! (call fconstrain i_min i_max . (+ err))
  i_term  <- liftA2 (*) (pid~>*pid_iGain) (pid~>*pid_iState)

  reset      <- pid~>*pid_reset
  d_term_var <- local (ival 0)

  ifte (reset /=? 0)
    (store (pid~>pid_reset) 0)
    (do d_state <- pid~>*pid_dState
        d_gain  <- pid~>*pid_dGain
        store d_term_var (d_gain * (pos - d_state)))
  store (pid~>pid_dState) pos

  d_term <- deref d_term_var
  ret $ p_term + i_term - d_term

-- | Define a group of parameters for a PID controller.
pid_param_init :: String -> Ref Global (Struct "PID") -> Ivory s r ()
pid_param_init name pid = do
  param_init (fromString $ name ++ "_P")    (pid ~> pid_pGain)
  param_init (fromString $ name ++ "_I")    (pid ~> pid_iGain)
  param_init (fromString $ name ++ "_D")    (pid ~> pid_dGain)
  param_init (fromString $ name ++ "_IMAX") (pid ~> pid_iMax)

instance ParamInit (Struct "PID") where
  param_init = pid_param_init

----------------------------------------------------------------------
-- Stabilization

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
   sensor_rate_rad_s max_servo_rate_rad_s -> body $ do
  stick_angle_deg   <- assign $ stick_angle_norm * max_stick_angle_deg
  sensor_angle_deg  <- assign $ degrees sensor_angle_rad
  angle_error       <- assign $ stick_angle_deg - sensor_angle_deg
  rate_deg_s        <- call pid_update angle_pid angle_error sensor_angle_deg
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s
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
   sensor_rate_rad_s max_servo_rate_rad_s -> body $ do
  stick_rate_deg_s  <- assign $ stick_rate_norm * max_stick_rate_deg_s
  sensor_rate_deg_s <- assign $ degrees sensor_rate_rad_s
  rate_error        <- assign $ stick_rate_deg_s - sensor_rate_deg_s
  servo_rate_deg_s  <- call pid_update rate_pid rate_error sensor_rate_deg_s
  servo_rate_norm   <- call fconstrain (-max_servo_rate_rad_s)
                            max_servo_rate_rad_s servo_rate_deg_s
  ret $ servo_rate_norm / max_servo_rate_rad_s
