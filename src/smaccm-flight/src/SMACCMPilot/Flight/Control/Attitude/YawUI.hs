{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Attitude.YawUI
  ( YawUI(..)
  , monitorYawUI
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.PID (fconstrain)

import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult    as S
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput        as UI
import qualified SMACCMPilot.Comm.Ivory.Types.AttControlDebug  as D

data YawUI =
  YawUI
    { yui_update :: forall eff s1 s2
                  . Ref s1 (Struct "sensors_result")
                 -> Ref s2 (Struct "user_input")
                 -> IFloat -- dt
                 -> Ivory eff ()
    , yui_reset :: forall eff . Ivory eff ()
    , yui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , yui_write_debug :: forall eff s . Ref s (Struct "att_control_debug")
                     -> Ivory eff ()
    }

monitorYawUI :: Monitor e YawUI
monitorYawUI = do
  let named n = fmap showUnique $ freshname $ "yaw_ui_" ++ n
  head_setpoint <- state "head_setpoint"
  rate_setpoint <- state "rate_setpoint"
  active_state <- stateInit "active_state" (ival false)
  name_update <- named "update"
  let proc_update :: Def('[ Ref s1 (Struct "sensors_result")
                          , Ref s2 (Struct "user_input")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc name_update $
        \sens ui dt -> body $ do
          -- Parameter in degrees/second, convert to rad/sec
          sensitivity <- assign (sens_dps *(pi/180))
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

  monitorModuleDef $ do
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
  sens_dps = 180.0

stickrate :: (GetAlloc eff ~ Scope cs)
          => Ref s (Struct "user_input")
          -> IFloat
          -> Ivory eff IFloat
stickrate ui sens = do
  stick_yaw  <- deref (ui ~> UI.yaw)
  offset     <- assign (signum stick_yaw * dead)
  scale      <- assign (sens / (1.0 - dead))
  stick_rate <- assign ((abs stick_yaw <? dead)
                  ? (0.0, (stick_yaw - offset) * scale))
  return stick_rate
  where
  dead = 0.30

-- Take an angle in radians which is outside of linear range (-pi, pi] by less
-- than pi, and maps it to an equal angle in that range.
angledomain :: IFloat -> IFloat
angledomain a =
  ( a <=? (-pi) ) ? ( a + 2*pi
  , (a >? pi ) ? ( a - 2*pi
    , a))



