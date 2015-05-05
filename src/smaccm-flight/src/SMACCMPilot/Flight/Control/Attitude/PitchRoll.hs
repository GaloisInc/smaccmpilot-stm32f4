{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.PitchRoll
  ( PitchRollControl(..)
  , monitorPitchRollControl
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Control.Attitude.Angle

import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
import qualified SMACCMPilot.Comm.Ivory.Types.ControlOutput   as OUT
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz             as XYZ
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

const_MAX_OUTPUT_ROLL :: IFloat
const_MAX_OUTPUT_ROLL  = 50 -- deg

data PitchRollControl =
  PitchRollControl
    { prc_init  :: forall eff . Ivory eff ()
    , prc_run   :: forall eff s . IFloat -- Pitch, Radians
                               -> IFloat -- Roll, Radians
                               -> ConstRef s (Struct "sensors_result")
                               -> Ivory eff ()
    , prc_state :: forall eff s . Ref s (Struct "control_output") -> Ivory eff ()
    , prc_reset :: forall eff . Ivory eff ()
    }

monitorPitchRollControl :: (AttrReadable a)
                        => ControllableVehicleAttrs a
                        -> Monitor e PitchRollControl
monitorPitchRollControl attrs = do
  pitch_ctl <- monitorAngleController (attitudePitchStab attrs)
                                       const_MAX_OUTPUT_ROLL
                                       "pitch"
  roll_ctl  <- monitorAngleController (attitudeRollStab attrs)
                                       const_MAX_OUTPUT_ROLL
                                       "roll"
  let named n = fmap showUnique $ freshname $ "pitchrollctl_" ++ n

  init_name <- named "init"
  run_name <- named "run"
  state_name <- named "state"
  reset_name <- named "reset"

  let init_proc :: Def ('[]:->())
      init_proc = proc init_name $ body $ do
        ac_reset pitch_ctl
        ac_reset roll_ctl

      run_proc :: Def ('[ IFloat
                        , IFloat
                        , ConstRef s2 (Struct "sensors_result")
                        ] :-> ())
      run_proc = proc run_name $ \pitch_setpt roll_setpt sens -> body $ do
        sen_roll    <- deref  (sens ~> S.roll)
        sen_pitch   <- deref  (sens ~> S.pitch)
        sen_omega_x <- deref ((sens ~> S.omega) ~> XYZ.x)
        sen_omega_y <- deref ((sens ~> S.omega) ~> XYZ.y)
        ac_run pitch_ctl (-1*pitch_setpt) sen_pitch sen_omega_y
        ac_run roll_ctl  roll_setpt       sen_roll  sen_omega_x

      state_proc :: Def ('[ Ref s1 (Struct "control_output")
                          ] :-> ())
      state_proc = proc state_name $ \out -> body $ do
          ac_out pitch_ctl >>= store (out ~> OUT.pitch)
          ac_out roll_ctl  >>= store (out ~> OUT.roll)

      reset_proc :: Def ('[]:->())
      reset_proc = proc reset_name $ body $ do
        ac_reset pitch_ctl
        ac_reset roll_ctl

  monitorModuleDef $ do
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

