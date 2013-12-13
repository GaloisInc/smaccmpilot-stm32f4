{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleUI where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.Altitude.Estimator
import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.AltControlDebug as D
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data ThrottleUI =
  ThrottleUI
    { ui_update :: forall eff s
                 . IBool -- autothrottle enabled
                -> Ref s (Struct "userinput_result")
                -> IFloat -- dt
                -> Ivory eff ()
    , ui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , ui_write_debug :: forall eff s . Ref s (Struct "alt_control_dbg")
                     -> Ivory eff ()
    }

taskThrottleUI :: ThrUIParams ParamReader -> AltEstimator -> Task p ThrottleUI
taskThrottleUI params estimator = do
  uniq <- fresh
  alt_setpoint <- taskLocal "alt_setpoint"
  vel_setpoint <- taskLocal "vel_setpoint"
  active_state <- taskLocalInit "active_state" (ival false)
  let proc_update :: Def('[ IBool
                          , Ref s2 (Struct "userinput_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc ("throttle_ui_update" ++ show uniq) $
        \enabled ui dt -> body $ do
          sr <- stickrate params ui
          store vel_setpoint sr

          active <- deref active_state
          (alt_est, _) <- ae_state estimator
          cond_
            [ iNot enabled
                ==> store active_state false
            , iNot active ==> do
                store alt_setpoint alt_est
                store active_state true
            , active ==> do
                current <- deref alt_setpoint
                next <- assign (current + (sr * dt))
                sens <- paramGet (thrUIsens params)
                a <- call fconstrain (alt_est - 0.8*sens)
                                     (alt_est + 0.8*sens)
                                     next
                store alt_setpoint a
            ]

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
        store (dbg ~> D.ui_setp) a
        store (dbg ~> D.ui_rate_setp) v
    }


stickrate :: (GetAlloc eff ~ Scope cs)
          => ThrUIParams ParamReader
          -> Ref s (Struct "userinput_result")
          -> Ivory eff IFloat
stickrate params ui = do
  sens       <- paramGet (thrUIsens params)
  dead       <- paramGet (thrUIdead params)
  stick_thr  <- deref (ui ~> UI.throttle)
  offset     <- assign (signum stick_thr * dead)
  scale      <- assign (sens / (1.0 - dead))
  stick_rate <- assign ((abs stick_thr <? dead)
                  ? (0.0, (stick_thr - offset) * scale))
  return stick_rate

