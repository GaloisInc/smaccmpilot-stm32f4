{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.PitchRoll
  ( PitchRollControl(..)
  , taskPitchRollControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import SMACCMPilot.Flight.Control.Attitude.Angle

import qualified SMACCMPilot.Flight.Types.Sensors       as SEN
import qualified SMACCMPilot.Flight.Types.ControlOutput as OUT

const_MAX_OUTPUT_ROLL :: IFloat
const_MAX_OUTPUT_ROLL  = 50 -- deg

data PitchRollControl =
  PitchRollControl
    { prc_init  :: forall eff . Ivory eff ()
    , prc_run   :: forall eff s . IFloat -- Pitch, Radians
                               -> IFloat -- Roll, Radians
                               -> ConstRef s (Struct "sensors_result")
                               -> Ivory eff ()
    , prc_state :: forall eff s . Ref s (Struct "controloutput") -> Ivory eff ()
    , prc_reset :: forall eff . Ivory eff ()
    }

taskPitchRollControl :: FlightParams ParamReader -> Task p PitchRollControl
taskPitchRollControl params = do
  f <- fresh
  pitch_ctl <- taskAngleController (flightPitch params)
                                   const_MAX_OUTPUT_ROLL
                                   "pitch"
  roll_ctl  <- taskAngleController (flightRoll params)
                                   const_MAX_OUTPUT_ROLL
                                   "roll"
  let named n = "pitchrollctl_" ++ n ++ "_" ++ (show f)

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        ac_reset pitch_ctl
        ac_reset roll_ctl

      run_proc :: Def ('[ IFloat
                        , IFloat
                        , ConstRef s2 (Struct "sensors_result")
                        ] :-> ())
      run_proc = proc (named "run") $ \pitch_setpt roll_setpt sens -> body $ do
        sen_roll    <- (sens ~>* SEN.roll)
        sen_pitch   <- (sens ~>* SEN.pitch)
        sen_omega_x <- (sens ~>* SEN.omega_x)
        sen_omega_y <- (sens ~>* SEN.omega_y)
        ac_run pitch_ctl (-1*pitch_setpt) sen_pitch sen_omega_y
        ac_run roll_ctl  roll_setpt       sen_roll  sen_omega_x

      state_proc :: Def ('[ Ref s1 (Struct "controloutput")
                          ] :-> ())
      state_proc = proc (named "state") $ \out -> body $ do
          ac_out pitch_ctl >>= store (out ~> OUT.pitch)
          ac_out roll_ctl  >>= store (out ~> OUT.roll)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        ac_reset pitch_ctl
        ac_reset roll_ctl

  taskModuleDef $ do
    incl init_proc
    incl run_proc
    incl state_proc
    incl reset_proc

  return PitchRollControl
    { prc_init  = call_ init_proc
    , prc_run   = call_ run_proc
    , prc_state = call_ state_proc
    , prc_reset = call_ reset_proc
    }

