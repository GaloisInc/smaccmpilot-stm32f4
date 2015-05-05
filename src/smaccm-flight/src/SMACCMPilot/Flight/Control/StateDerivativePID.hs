{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.StateDerivativePID
  ( StateDerivativePID(..)
  , monitorStateDerivativePID
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig as C
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Flight.Control.PID

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

monitorStateDerivativePID :: (AttrReadable a)
                          => a (Struct "pid_config")
                          -> String
                          -> Monitor e StateDerivativePID
monitorStateDerivativePID config_attr username = do
  integral   <- state (username ++ "_integral")
  p_out      <- state (username ++ "_p_out")
  d_out      <- state (username ++ "_d_out")
  cfg        <- attrState config_attr

  let named n = fmap showUnique $ freshname $ "statepid_" ++ username ++ "_" ++ n

  update_name <- named "update"
  output_name <- named "output"
  reset_name <- named "reset"

  let update_proc :: Def ('[ IFloat
                           , IFloat
                           , IFloat
                           , IFloat
                           ] :-> ())
      update_proc = proc update_name $ \setpt state_est deriv_est dt -> body $ do
        assert (dt >? 0)
        p_gain <-             (deref (cfg ~> C.p_gain))
        i_gain <- fmap (* dt) (deref (cfg ~> C.i_gain))
        d_gain <- fmap (/ dt) (deref (cfg ~> C.d_gain))
        i_max  <-             (deref (cfg ~> C.i_max))

        err <- assign (setpt - state_est)

        i_prev <- deref integral
        i <- call fconstrain (-1*i_max) i_max (i_prev + (i_gain * err))
        store integral i

        store p_out (err * p_gain)
        store d_out (deriv_est * d_gain)

      output_proc :: Def ('[]:->IFloat)
      output_proc = proc output_name $ body $ do
        p <- deref p_out
        i <- deref integral
        d <- deref d_out
        ret (p + i - d)

      reset_proc :: Def ('[]:->())
      reset_proc = proc reset_name $ body $ do
        store integral 0

  monitorModuleDef $ do
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


