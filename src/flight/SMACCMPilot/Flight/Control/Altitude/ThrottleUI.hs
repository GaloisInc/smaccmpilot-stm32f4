{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleUI where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import qualified SMACCMPilot.Flight.Types.AltControlDebug as A
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import           SMACCMPilot.Flight.Types.Armed
import           SMACCMPilot.Flight.Types.FlightModeData
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data ThrottleUI =
  ThrottleUI 
    { ui_update :: forall eff s
                 . FlightMode
                -> ArmedMode
                -> Ref s (Struct "userinput_result")
                -> IFloat -- dt
                -> Ivory eff ()
    , ui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , ui_write_debug :: forall eff s . Ref s (Struct "alt_control_dbg")
                     -> Ivory eff ()
    }

taskThrottleUI :: ThrUIParams ParamReader -> AltEstimator -> Task p ThrottleUI
taskThrottleUI params _estimator = do
  uniq <- fresh
  alt_setpoint <- taskLocal "alt_setpoint"
  vel_setpoint <- taskLocal "vel_setpoint"
  let proc_update :: Def('[ FlightMode
                          , ArmedMode
                          , Ref s (Struct "userinput_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc ("throttle_ui_update" ++ show uniq) $
        \fm ar ui dt -> body $ do
          sens       <- paramGet (thrUIsens params)
          dead       <- paramGet (thrUIdead params)
          stick_thr  <- deref (ui ~> UI.throttle)
          offset     <- assign (signum stick_thr * dead)
          scale      <- assign (sens / (1.0 - dead))
          stick_rate <- assign ((abs stick_thr <? dead)
                          ? (0.0, (stick_thr - offset) * scale))
          store vel_setpoint stick_rate
          -- TODO: integrate velocity setpoint into altitude
          -- setpoint, with logic for resetting integral when
          -- disarmed or flightmode changed. Also, limit integral
          -- to be within a certain distance of current altitude,
          -- and when resetting integral, offset from current altitude
          -- by ~0.5..1s * current alt rate, to reduce oscillation
  taskModuleDef $ incl proc_update
  return ThrottleUI
    { ui_update   = call_ proc_update
    , ui_setpoint = do
        a <- deref alt_setpoint
        v <- deref vel_setpoint
        return (a,v)
    , ui_write_debug = \dbg -> do
        a <- deref alt_setpoint
        v <- deref vel_setpoint
        store (dbg ~> A.ui_setp) a
        store (dbg ~> A.ui_rate_setp) v
    }

