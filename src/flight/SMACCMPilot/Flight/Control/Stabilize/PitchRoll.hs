{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Stabilize.PitchRoll
  ( PitchRollControl(..)
  , taskPitchRollControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Flight.Types.Sensors       as SEN
import qualified SMACCMPilot.Flight.Types.ControlOutput as OUT

const_MAX_INPUT_ROLL :: IFloat
const_MAX_INPUT_ROLL  = 45  -- deg

const_MAX_OUTPUT_ROLL :: IFloat
const_MAX_OUTPUT_ROLL  = 50 -- deg

data PitchRollControl =
  PitchRollControl
    { prc_init  :: forall eff . Ivory eff ()
    , prc_run   :: forall eff s . IFloat -- Pitch
                               -> IFloat -- Roll
                               -> ConstRef s (Struct "sensors_result")
                               -> Ivory eff ()
    , prc_state :: forall eff s . Ref s (Struct "controloutput") -> Ivory eff ()
    , prc_reset :: forall eff . Ivory eff ()
    }

taskPitchRollControl :: FlightParams ParamReader -> Task p PitchRollControl
taskPitchRollControl params = do
  f <- fresh
  roll_stab  <- taskLocal "roll_stab"
  roll_rate  <- taskLocal "roll_rate"
  pitch_stab <- taskLocal "pitch_stab"
  pitch_rate <- taskLocal "pitch_rate"

  valid      <- taskLocal "valid"
  roll_out   <- taskLocal "roll_out"
  pitch_out  <- taskLocal "pitch_out"

  let named n = "pitchrollctl_" ++ n ++ "_" ++ (show f)

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        store valid false
        call_ reset_proc

      run_proc :: Def ('[ IFloat
                        , IFloat
                        , ConstRef s2 (Struct "sensors_result")
                        ] :-> ())
      run_proc = proc (named "run") $ \pitch_setpt roll_setpt sens -> body $ do
        roll_stabilize_cfg  <- allocPIDParams (flightRollStab  params)
        roll_rate_cfg       <- allocPIDParams (flightRollRate  params)
        pitch_stabilize_cfg <- allocPIDParams (flightPitchStab params)
        pitch_rate_cfg      <- allocPIDParams (flightPitchRate params)

        sen_roll    <- (sens ~>* SEN.roll)
        sen_pitch   <- (sens ~>* SEN.pitch)
        sen_omega_x <- (sens ~>* SEN.omega_x)
        sen_omega_y <- (sens ~>* SEN.omega_y)

        roll_ctl <- call stabilize_from_angle
                            roll_stab
                            (constRef roll_stabilize_cfg)
                            roll_rate
                            (constRef roll_rate_cfg)
                            roll_setpt
                            const_MAX_INPUT_ROLL
                            sen_roll
                            sen_omega_x
                            const_MAX_OUTPUT_ROLL

        pitch_ctl <- call stabilize_from_angle
                            pitch_stab
                            (constRef pitch_stabilize_cfg)
                            pitch_rate
                            (constRef pitch_rate_cfg)
                            (-1 * pitch_setpt)
                            const_MAX_INPUT_ROLL
                            sen_pitch
                            sen_omega_y
                            const_MAX_OUTPUT_ROLL
        store valid true
        store roll_out  roll_ctl
        store pitch_out pitch_ctl

      state_proc :: Def ('[ Ref s1 (Struct "controloutput")
                          ] :-> ())
      state_proc = proc (named "state") $ \out -> body $ do
        v <- deref valid
        ifte_ v
          (deref roll_out  >>= store (out ~> OUT.roll) >>
           deref pitch_out >>= store (out ~> OUT.pitch))
          (store (out ~> OUT.roll)  0 >>
           store (out ~> OUT.pitch) 0)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        store valid false
        mapM_ (call_ pid_reset)
          [roll_stab, roll_rate, pitch_stab, pitch_rate]

  taskModuleDef $ do
    incl init_proc
    incl run_proc
    incl state_proc
    incl reset_proc
    incl stabilize_from_angle
    depend controlPIDModule
  return PitchRollControl
    { prc_init  = call_ init_proc
    , prc_run   = call_ run_proc
    , prc_state = call_ state_proc
    , prc_reset = call_ reset_proc
    }

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

-- | Convert an angle in radians to degrees.
degrees :: (Fractional a) => a -> a
degrees x = x * 57.295779513082320876798154814105
