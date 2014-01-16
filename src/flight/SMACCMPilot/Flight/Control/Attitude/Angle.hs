{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Attitude.Angle
  ( AngleControl(..)
  , taskAngleController
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Control.Attitude.Stabilize

data AngleControl =
  AngleControl
    { ac_init  :: forall eff . Ivory eff ()
    , ac_run   :: forall eff . IFloat -- Target, radians
                            -> IFloat -- Current estimate, radians
                            -> IFloat -- Derivative estimate, radians per second
                            -> Ivory eff ()
    , ac_out   :: forall eff . Ivory eff IFloat
    , ac_reset :: forall eff . Ivory eff ()
    }

taskAngleController :: StabilizerParams ParamReader
                    -> IFloat -- output range (absolute value)
                    -> String                -- name
                    -> Task p AngleControl
taskAngleController stab_params output_range name = do
  f <- fresh
  let named n = name ++ "_anglectl_" ++ n ++ "_" ++ (show f)

  valid    <- taskLocal "valid"
  out      <- taskLocal "out"
  pos_pid  <- taskLocal "pos_pid"
  rate_pid <- taskLocal "rate_pid"

  let init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        store valid false
        call_ reset_proc

      run_proc :: Def ('[IFloat, IFloat, IFloat] :-> ())
      run_proc = proc (named "run") $ \setpt est deriv_est -> body $ do
        pos_cfg  <- allocPIDParams (stabPosition stab_params)
        rate_cfg <- allocPIDParams (stabRate     stab_params)
        ctl <- call stabilize_from_angle
                      pos_pid
                      (constRef pos_cfg)
                      rate_pid
                      (constRef rate_cfg)
                      setpt
                      est
                      deriv_est
                      output_range
        store valid true
        store out ctl

      out_proc :: Def ('[] :-> IFloat)
      out_proc = proc (named "out") $ body $ do
        v <- deref valid
        ifte_ v
          (deref out >>= ret)
          (ret 0)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        store valid false
        call_ pid_reset pos_pid
        call_ pid_reset rate_pid

  taskModuleDef $ do
    incl init_proc
    incl run_proc
    incl out_proc
    incl reset_proc
    depend controlPIDModule
    depend attStabilizeModule
  return AngleControl
    { ac_init  = call_ init_proc
    , ac_run   = call_ run_proc
    , ac_out   = call  out_proc
    , ac_reset = call_ reset_proc
    }
