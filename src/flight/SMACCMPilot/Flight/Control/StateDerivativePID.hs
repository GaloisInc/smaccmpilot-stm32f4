{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.StateDerivativePID
  ( StateDerivativePID(..)
  , taskStateDerivativePID
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Param

data StateDerivativePID =
  StateDerivativePID
    { sdpid_init   :: forall eff . Ivory eff ()
    , sdpid_update :: forall eff . IFloat -- Setpoint
                                -> IFloat -- State Estimate
                                -> IFloat -- State Derivative Estimate
                                -> IFloat -- dt
                                -> Ivory eff ()
    , sdpid_output :: forall eff . Ivory eff IFloat
    , sdpid_reset  :: forall eff . Ivory eff ()
    , sdpid_debug  :: forall eff . Ivory eff (IFloat, IFloat, IFloat)
    }

taskStateDerivativePID :: PIDParams ParamReader -> String -> Task p StateDerivativePID
taskStateDerivativePID params username = do
  f <- fresh
  integral   <- taskLocal (username ++ "_integral")
  p_out      <- taskLocal (username ++ "_p_out")
  d_out      <- taskLocal (username ++ "_d_out")

  let named n = "statepid_" ++ username ++ "_" ++ n ++ "_" ++ (show f)

      update_proc :: Def ('[ IFloat
                           , IFloat
                           , IFloat
                           , IFloat
                           ] :-> ())
      update_proc = proc (named "update") $ \setpt state_est deriv_est dt -> body $ do
        assert (dt >? 0)
        cfg <- allocPIDParams params
        p_gain <-             (deref (cfg ~> pid_pGain))
        i_gain <- fmap (* dt) (deref (cfg ~> pid_iGain))
        d_gain <- fmap (/ dt) (deref (cfg ~> pid_dGain))
        i_max  <- deref (cfg ~> pid_iMax)

        err <- assign (setpt - state_est)

        i_prev <- deref integral
        i <- call fconstrain (-1*i_max) i_max (i_prev + (i_gain * err))
        store integral i

        store p_out (err * p_gain)
        store d_out (deriv_est * d_gain)

      output_proc :: Def ('[]:->IFloat)
      output_proc = proc (named "output") $ body $ do
        p <- deref p_out
        i <- deref integral
        d <- deref d_out
        ret (p + i - d)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        store integral 0

  taskModuleDef $ do
    incl update_proc
    incl output_proc
    incl reset_proc
    depend controlPIDModule
  return StateDerivativePID
    { sdpid_init   = call_ reset_proc
    , sdpid_update = call_ update_proc
    , sdpid_output = call  output_proc
    , sdpid_reset  = call_ reset_proc
    , sdpid_debug  = do
        p <- deref p_out
        i <- deref integral
        d <- deref d_out
        return (p,i,d)
    }


