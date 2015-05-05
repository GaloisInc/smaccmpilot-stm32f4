{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.PositionPID where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.PidState        as P
import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig       as C
import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Control.Altitude.Estimator

data PositionPid =
  PositionPid
    { pos_pid_init :: forall eff . Ivory eff ()
    , pos_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , pos_pid_calculate :: forall eff . IFloat -- Setpoint
                                     -> IFloat -- Setpoint derivative
                                     -> IFloat -- dt, seconds
                                     -> Ivory eff IFloat
    , pos_pid_write_debug :: forall eff s .
          Ref s (Struct "alt_control_debug") -> Ivory eff ()
    }

monitorPositionPid :: AttrReadable a
                   => a (Struct "pid_config")
                   -> AltEstimator
                   -> Monitor e PositionPid
monitorPositionPid pid_config alt_estimator = do
  ppid_state  <- state "posPidState"
  ppid_config <- attrState pid_config
  name_pid_calculate <- fmap showUnique $ freshname "pos_pid_calculate"
  let proc_pid_calculate :: Def('[IFloat, IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc name_pid_calculate $
        \pos_sp vel_sp dt -> body $ do
          p_gain <-             (ppid_config~>*C.p_gain)
          d_gain <- fmap (/ dt) (ppid_config~>*C.d_gain)

          (pos_est, vel_est) <- ae_state alt_estimator
          pos_err            <- assign (pos_sp - pos_est)
          vel_err            <- assign (vel_sp - vel_est)

          pid_result <- assign ((p_gain * pos_err) - (d_gain * vel_err))
          ret (pid_result + vel_sp)

  monitorModuleDef $ do
    incl proc_pid_calculate
    depend controlPIDModule
  return PositionPid
    { pos_pid_init = do
        call_ pid_reset ppid_state
    , pos_pid_set_integral = \i ->
        store (ppid_state ~> P.i_state) i
    , pos_pid_calculate = call proc_pid_calculate
    , pos_pid_write_debug = \r -> do
        refCopy (r ~> A.pos) ppid_state
    }

