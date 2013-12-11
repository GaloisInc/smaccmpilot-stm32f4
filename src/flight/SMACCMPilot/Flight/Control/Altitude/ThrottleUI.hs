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
import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.ThrottleMode    as TM
import qualified SMACCMPilot.Flight.Types.ArmedMode       as A
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Param

data ThrottleUI =
  ThrottleUI 
    { ui_update :: forall eff s1 s2
                 . Ref s1 (Struct "control_law")
                -> Ref s2 (Struct "userinput_result")
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
  let proc_update :: Def('[ Ref s1 (Struct "control_law")
                          , Ref s2 (Struct "userinput_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc ("throttle_ui_update" ++ show uniq) $
        \cl ui dt -> body $ do
          armed    <- deref (cl ~> CL.armed_mode)
          thr_mode <- deref (cl ~> CL.thr_mode)
          sr <- stickrate params ui
          store vel_setpoint sr

          active <- deref active_state
          (alt_est, _) <- ae_state estimator
          cond_
            [ armed /=? A.armed .|| thr_mode /=? TM.autothrottle
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

