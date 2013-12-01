{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrustPID where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data ThrustPid =
  ThrustPid
    { thrust_pid_init :: forall eff . Ivory eff ()
    , thrust_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , thrust_pid_calculate :: forall eff . IFloat -- Setpoint
                                        -> IFloat -- Estimate
                                        -> IFloat -- dt, seconds
                                        -> Ivory eff IFloat
    , thrust_pid_write_debug :: forall eff s .
          Ref s (Struct "alt_control_dbg") -> Ivory eff ()
    }

taskThrustPid :: PIDParams ParamReader -> Task p ThrustPid
taskThrustPid params = do
  uniq <- fresh
  tpid_state  <- taskLocal "thrustPidState"
  tpid_params <- taskLocal "thrustPidParams"
  let proc_pid_calculate :: Def('[IFloat, IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc ("thrust_pid_calculate" ++ show uniq) $
        \vel_sp vel_est dt -> body $ do
          getPIDParams params tpid_params
          (tpid_params ~> pid_iGain) %= (* dt)
          (tpid_params ~> pid_dGain) %= (/ dt)
          store (tpid_params ~> pid_iMin) 0.2 -- Nonstandard imax/imin range
          err     <- assign (vel_sp - vel_est)
          pid_out <- call pid_update tpid_state (constRef tpid_params) err vel_est
          out     <- call fconstrain 0.1 1.0 pid_out
          ret out
  taskModuleDef $ incl proc_pid_calculate
  return ThrustPid
    { thrust_pid_init = do
        call_ pid_reset tpid_state
    , thrust_pid_set_integral = \i ->
        store (tpid_state ~> pid_iState) i
    , thrust_pid_calculate = call proc_pid_calculate
    , thrust_pid_write_debug = \r -> do
        p <- deref (tpid_state ~> pid_pLast)
        i <- deref (tpid_state ~> pid_iState)
        d <- deref (tpid_state ~> pid_dLast)
        store (r ~> A.thrust_p) p
        store (r ~> A.thrust_i) i
        store (r ~> A.thrust_d) d
    }

