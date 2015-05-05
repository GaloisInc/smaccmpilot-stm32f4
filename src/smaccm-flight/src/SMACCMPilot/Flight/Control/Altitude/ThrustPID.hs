{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrustPID where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A
import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig       as C
import qualified SMACCMPilot.Comm.Ivory.Types.PidState        as P
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.PID

data ThrustPid =
  ThrustPid
    { thrust_pid_init :: forall eff . Ivory eff ()
    , thrust_pid_set_integral :: forall eff . IFloat -> Ivory eff ()
    , thrust_pid_calculate :: forall eff . IFloat -- Setpoint
                                        -> IFloat -- dt, seconds
                                        -> Ivory eff IFloat
    , thrust_pid_write_debug :: forall eff s .
          Ref s (Struct "alt_control_debug") -> Ivory eff ()
    }

monitorThrustPid :: (AttrReadable a)
                 => a (Struct "pid_config")
                 -> AltEstimator
                 -> Monitor e ThrustPid
monitorThrustPid config alt_estimator = do
  tpid_state  <- state "thrustPidState"
  tpid_config <- attrState config
  name_pid_calculate <- fmap showUnique $ freshname "thrust_pid_calculate"
  let proc_pid_calculate :: Def('[IFloat, IFloat] :-> IFloat)
      proc_pid_calculate = proc name_pid_calculate $
        \vel_sp dt -> body $ do
          pgain <- deref (tpid_config ~> C.p_gain)
          igain <- deref (tpid_config ~> C.i_gain)
          dgain <- deref (tpid_config ~> C.d_gain)
          tpid_params <- local (istruct [ C.i_gain .= ival (igain * dt)
                                        , C.d_gain .= ival (dgain / dt)
                                        , C.p_gain .= ival pgain
                                        , C.i_min  .= ival 0.2 -- Nonstandard imax/imin range
                                        , C.i_max  .= ival 1
                                        ])
          (_, vel_est) <- ae_state alt_estimator
          err     <- assign (vel_sp - vel_est)
          pid_out <- call pid_update tpid_state (constRef tpid_params) err vel_est
          out     <- call fconstrain 0.1 1.0 pid_out
          ret out
  monitorModuleDef $ incl proc_pid_calculate
  return ThrustPid
    { thrust_pid_init = do
        call_ pid_reset tpid_state
    , thrust_pid_set_integral = \i ->
        store (tpid_state ~> P.i_state) i
    , thrust_pid_calculate = call proc_pid_calculate
    , thrust_pid_write_debug = \r -> do
        refCopy (r ~> A.thrust) tpid_state
    }

