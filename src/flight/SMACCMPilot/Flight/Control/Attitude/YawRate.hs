{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.YawRate
  ( YawRateControl(..)
  , taskYawRateControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Control.Attitude.Stabilize
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Flight.Types.Sensors       as SEN
import qualified SMACCMPilot.Flight.Types.ControlOutput as OUT

data YawRateControl =
  YawRateControl
    { yc_init  :: forall eff . Ivory eff ()
    , yc_run   :: forall eff s . IFloat -- Yaw Rate
                              -> ConstRef s (Struct "sensors_result")
                              -> Ivory eff ()
    , yc_state :: forall eff s . Ref s (Struct "controloutput") -> Ivory eff ()
    , yc_reset :: forall eff . Ivory eff ()
    }

const_MAX_INPUT_YAW :: IFloat
const_MAX_INPUT_YAW   = 180 -- deg/sec
const_MAX_OUTPUT_YAW :: IFloat
const_MAX_OUTPUT_YAW   = 45 -- deg/sec

taskYawRateControl :: PIDParams ParamReader -> Task p YawRateControl
taskYawRateControl params = do
  f <- fresh
  yaw_rate  <- taskLocal "yaw_rate"

  valid     <- taskLocal "valid"
  yaw_out   <- taskLocal "yaw_out"

  let named n = "yawctl_" ++ n ++ "_" ++ (show f)

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        store valid false
        call_ reset_proc

      run_proc :: Def ('[ IFloat
                        , ConstRef s (Struct "sensors_result")
                        ] :-> ())
      run_proc = proc (named "run") $ \yaw_rate_setpt sens -> body $ do
        sen_omega_z <- (sens ~>* SEN.omega_z)
        yaw_rate_cfg <- allocPIDParams params
        yaw_ctl <- call stabilize_from_rate
                            yaw_rate
                            (constRef yaw_rate_cfg)
                            yaw_rate_setpt
                            const_MAX_INPUT_YAW
                            sen_omega_z
                            const_MAX_OUTPUT_YAW

        store valid true
        store yaw_out yaw_ctl

      state_proc :: Def ('[ Ref s1 (Struct "controloutput")
                          ] :-> ())
      state_proc = proc (named "state") $ \out -> body $ do
        v <- deref valid
        ifte_ v
          (deref yaw_out  >>= store (out ~> OUT.yaw))
          (store (out ~> OUT.yaw) 0)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        store valid false
        call_ pid_reset yaw_rate

  taskModuleDef $ do
    incl init_proc
    incl run_proc
    incl state_proc
    incl reset_proc
    depend controlPIDModule
    depend attStabilizeModule
  return YawRateControl
    { yc_init  = call_ init_proc
    , yc_run   = call_ run_proc
    , yc_state = call_ state_proc
    , yc_reset = call_ reset_proc
    }


