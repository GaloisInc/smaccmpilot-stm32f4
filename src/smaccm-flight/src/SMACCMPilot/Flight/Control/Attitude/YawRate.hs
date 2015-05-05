{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.YawRate
  ( YawRateControl(..)
  , monitorYawRateControl
  ) where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Control.Attitude.Stabilize
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult as SEN
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz           as XYZ
import           SMACCMPilot.Comm.Tower.Attr


data YawRateControl =
  YawRateControl
    { yc_init  :: forall eff . Ivory eff ()
    , yc_run   :: forall eff s . IFloat -- Yaw Rate
                              -> ConstRef s (Struct "sensors_result")
                              -> Ivory eff ()
    , yc_state :: forall eff . Ivory eff IFloat
    , yc_reset :: forall eff . Ivory eff ()
    }

const_MAX_OUTPUT_YAW :: IFloat
const_MAX_OUTPUT_YAW   = 45 -- deg/sec

monitorYawRateControl :: (AttrReadable a)
                      => a (Struct "pid_config")
                      -> Monitor p YawRateControl
monitorYawRateControl config_attr = do
  yaw_rate  <- state "yaw_rate"

  valid     <- state "valid"
  yaw_out   <- state "yaw_out"
  yaw_rate_cfg <- attrState config_attr
  let named n = fmap showUnique $ freshname $ "yawctl_" ++ n

  init_name <- named "init"
  run_name <- named "run"
  state_name <- named "state"
  reset_name <- named "reset"

  let init_proc :: Def ('[]:->())
      init_proc = proc init_name $ body $ do
        store valid false
        call_ reset_proc

      run_proc :: Def ('[ IFloat
                        , ConstRef s (Struct "sensors_result")
                        ] :-> ())
      run_proc = proc run_name $ \yaw_rate_setpt sens -> body $ do
        sen_omega_z <- deref ((sens ~> SEN.omega) ~> XYZ.z)
        yaw_ctl <- call stabilize_from_rate
                            yaw_rate
                            (constRef yaw_rate_cfg)
                            yaw_rate_setpt
                            sen_omega_z
                            const_MAX_OUTPUT_YAW

        store valid true
        store yaw_out yaw_ctl

      state_proc :: Def ('[] :-> IFloat)
      state_proc = proc state_name $ body $ do
        v <- deref valid
        ifte_ v
          (deref yaw_out >>= ret)
          (ret 0)

      reset_proc :: Def ('[]:->())
      reset_proc = proc reset_name $ body $ do
        store valid false
        call_ pid_reset yaw_rate

  monitorModuleDef $ do
    incl init_proc
    incl run_proc
    incl state_proc
    incl reset_proc
    depend controlPIDModule
    depend attStabilizeModule
  return YawRateControl
    { yc_init  = call_ init_proc
    , yc_run   = call_ run_proc
    , yc_state = call  state_proc
    , yc_reset = call_ reset_proc
    }


