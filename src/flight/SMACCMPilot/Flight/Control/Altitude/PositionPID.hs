{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.PositionPID where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data PositionPid =
  PositionPid
    { pos_pid_init :: forall eff . Ivory eff ()
    , pos_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , pos_pid_calculate :: forall eff . IFloat -- Set position
                                     -> IFloat -- Set derivative
                                     -> IFloat -- Estimate
                                     -> IFloat -- dt, seconds
                                     -> Ivory eff IFloat
    , pos_pid_write_debug :: forall eff s .
          Ref s (Struct "alt_control_dbg") -> Ivory eff ()
    }

taskPositionPid :: PIDParams ParamReader -> Task p PositionPid
taskPositionPid params = do
  uniq <- fresh
  ppid_state  <- taskLocal "posPidState"
  ppid_params <- taskLocal "posPidParams"
  let proc_pid_calculate :: Def('[IFloat, IFloat, IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc ("pos_pid_calculate" ++ show uniq) $
        \pos_sp vel_sp pos_est dt -> body $ do
          getPIDParams params ppid_params
          p_gain <-             (ppid_params~>*pid_pGain)
          i_gain <- fmap (* dt) (ppid_params~>*pid_iGain)
          i_min  <-             (ppid_params~>*pid_iMin)
          i_max  <-             (ppid_params~>*pid_iMax)
          d_gain <- fmap (/ dt) (ppid_params~>*pid_dGain)

          err     <- assign (pos_sp - pos_est)
          -- XXX totally unimplemented...
          ret vel_sp

  taskModuleDef $ incl proc_pid_calculate
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

