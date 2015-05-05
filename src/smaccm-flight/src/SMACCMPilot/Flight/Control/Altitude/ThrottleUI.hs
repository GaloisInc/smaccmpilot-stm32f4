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

import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as D
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput       as UI
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleUi      as TUI
import           SMACCMPilot.Comm.Tower.Attr

data ThrottleUI =
  ThrottleUI
    { tui_update :: forall eff s
                 . Ref s (Struct "user_input")
                -> IFloat -- dt
                -> Ivory eff ()
    , tui_reset  :: forall eff . Ivory eff ()
    , tui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , tui_write_debug :: forall eff s . Ref s (Struct "alt_control_debug")
                     -> Ivory eff ()
    }

monitorThrottleUI :: (AttrReadable a)
                  => a (Struct "throttle_ui")
                  -> AltEstimator
                  -> Monitor p ThrottleUI
monitorThrottleUI attr estimator = do
  alt_setpoint <- state "alt_setpoint"
  vel_setpoint <- state "vel_setpoint"
  active_state <- stateInit "active_state" (ival false)
  settings     <- attrState attr
  name_update <- fmap showUnique $ freshname "throttle_ui_update"
  let proc_update :: Def('[ Ref s (Struct "user_input")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc name_update $
        \ui dt -> body $ do
          sr <- stickrate settings ui
          store vel_setpoint sr

          active <- deref active_state
          (alt_est, _) <- ae_state estimator
          cond_
            [ iNot active ==> do
                store alt_setpoint alt_est
                store active_state true
            , active ==> do
                current <- deref alt_setpoint
                next <- assign (current + (sr * dt))
                sens <- deref (settings ~> TUI.sens)
                a <- call fconstrain (alt_est - 0.8*sens)
                                     (alt_est + 0.8*sens)
                                     next
                store alt_setpoint a
            ]

  monitorModuleDef $ incl proc_update
  return ThrottleUI
    { tui_update   = call_ proc_update
    , tui_reset    = store active_state false
    , tui_setpoint = do
        a <- deref alt_setpoint
        v <- deref vel_setpoint
        return (a,v)
    , tui_write_debug = \dbg -> do
        a <- deref alt_setpoint
        v <- deref vel_setpoint
        store (dbg ~> D.ui_setp) a
        store (dbg ~> D.ui_rate_setp) v
    }


stickrate :: (GetAlloc eff ~ Scope cs)
          => Ref s1 (Struct "throttle_ui")
          -> Ref s2 (Struct "user_input")
          -> Ivory eff IFloat
stickrate settings ui = do
  sens       <- deref (settings ~> TUI.sens)
  dead       <- deref (settings ~> TUI.dead)
  stick_thr  <- deref (ui ~> UI.throttle)
  offset     <- assign (signum stick_thr * dead)
  scale      <- assign (sens / (1.0 - dead))
  stick_rate <- assign ((abs stick_thr <? dead)
                  ? (0.0, (stick_thr - offset) * scale))
  return stick_rate

