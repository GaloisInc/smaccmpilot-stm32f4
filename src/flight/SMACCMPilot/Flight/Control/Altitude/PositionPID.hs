{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.PositionPID where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data PositionPid =
  PositionPid
    { pos_pid_init :: forall eff . Ivory eff ()
    , pos_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , pos_pid_calculate :: forall eff . IFloat -- Setpoint
                                     -> IFloat -- Setpoint derivative
                                     -> IFloat -- dt, seconds
                                     -> Ivory eff IFloat
    , pos_pid_write_debug :: forall eff s .
          Ref s (Struct "alt_control_dbg") -> Ivory eff ()
    }

taskPositionPid :: PIDParams ParamReader -> AltEstimator -> Task p PositionPid
taskPositionPid params alt_estimator = do
  uniq <- fresh
  ppid_state  <- taskLocal "posPidState"
  let proc_pid_calculate :: Def('[IFloat, IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc ("pos_pid_calculate" ++ show uniq) $
        \pos_sp vel_sp dt -> body $ do
          ppid_params <- allocPIDParams params
          p_gain <-             (ppid_params~>*pid_pGain)
          d_gain <- fmap (/ dt) (ppid_params~>*pid_dGain)

          (pos_est, vel_est) <- ae_state alt_estimator
          pos_err            <- assign (pos_sp - pos_est)
          vel_err            <- assign (vel_sp - vel_est)

          pid_result <- assign ((p_gain * pos_err) - (d_gain * vel_err))
          ret (pid_result + vel_sp)

  taskModuleDef $ do
    incl proc_pid_calculate
    depend controlPIDModule
  return PositionPid
    { pos_pid_init = do
        call_ pid_reset ppid_state
    , pos_pid_set_integral = \i ->
        store (ppid_state ~> pid_iState) i
    , pos_pid_calculate = call proc_pid_calculate
    , pos_pid_write_debug = \r -> do
        p <- deref (ppid_state ~> pid_pLast)
        i <- deref (ppid_state ~> pid_iState)
        d <- deref (ppid_state ~> pid_dLast)
        store (r ~> A.pos_p) p
        store (r ~> A.pos_i) i
        store (r ~> A.pos_d) d
    }

