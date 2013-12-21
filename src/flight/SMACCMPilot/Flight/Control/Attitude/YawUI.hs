{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Attitude.YawUI where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Flight.Control.PID (fconstrain)
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import qualified SMACCMPilot.Flight.Types.AttControlDebug as D

data YawUI =
  YawUI
    { yui_update :: forall eff s1 s2
                  . Ref s1 (Struct "sensors_result")
                 -> Ref s2 (Struct "userinput_result")
                 -> IFloat -- dt
                 -> Ivory eff ()
    , yui_reset :: forall eff . Ivory eff ()
    , yui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , yui_write_debug :: forall eff s . Ref s (Struct "att_control_dbg")
                     -> Ivory eff ()
    }

taskYawUI :: Task p YawUI
taskYawUI = do
  uniq <- fresh
  let named n = "yaw_ui_" ++ n ++ "_" ++ show uniq
  head_setpoint <- taskLocal "head_setpoint"
  rate_setpoint <- taskLocal "rate_setpoint"
  active_state <- taskLocalInit "active_state" (ival false)
  let proc_update :: Def('[ Ref s1 (Struct "sensors_result")
                          , Ref s2 (Struct "userinput_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc (named "update") $
        \sens ui dt -> body $ do
          sr <- stickrate ui sensitivity
          store rate_setpoint sr

          active <- deref active_state
          head_est <- deref (sens ~> S.yaw)
          unless active $ do
            store head_setpoint head_est
            store active_state true
          when active $ do
            current <- deref head_setpoint
            next <- assign (current + (angledomain (sr * dt)))
            a <- call fconstrain (head_est - 0.8*sensitivity)
                                 (head_est + 0.8*sensitivity)
                                 next
            store head_setpoint (angledomain a)

  taskModuleDef $ do
    incl proc_update
  return YawUI
    { yui_update   = call_ proc_update
    , yui_reset    = store active_state false
    , yui_setpoint = do
        h <- deref head_setpoint
        r <- deref rate_setpoint
        return (h,r)
    , yui_write_debug = \d -> do
        h <- deref head_setpoint
        r <- deref rate_setpoint
        store (d ~> D.head_setpt) h
        store (d ~> D.head_rate_setpt) r
    }
  where
  sensitivity = pi / 4

stickrate :: (GetAlloc eff ~ Scope cs)
          => Ref s (Struct "userinput_result")
          -> IFloat
          -> Ivory eff IFloat
stickrate ui sens = do
  dead       <- assign 0.30
  stick_thr  <- deref (ui ~> UI.throttle)
  offset     <- assign (signum stick_thr * dead)
  scale      <- assign (sens / (1.0 - dead))
  stick_rate <- assign ((abs stick_thr <? dead)
                  ? (0.0, (stick_thr - offset) * scale))
  return stick_rate

-- Take an angle in radians which is outside of linear range (-pi, pi] by less
-- than pi, and maps it to an equal angle in that range.
angledomain :: IFloat -> IFloat
angledomain a =
  ( a <=? (-pi) ) ? ( a + 2*pi
  , (a >? pi ) ? ( a - 2*pi
    , a))



