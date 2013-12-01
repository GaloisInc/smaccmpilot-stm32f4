{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleUI where

import Ivory.Language
import Ivory.Tower

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
                -> IFloat -- Altitude estimate
                -> IFloat -- Altitude rate estimate
                -> Ivory eff ()
    , ui_setpoint :: forall eff . Ivory eff (IFloat, IFloat)
    , ui_write_debug :: forall eff s . Ref s (Struct "alt_controller_dbg")
                     -> Ivory eff ()
    }

taskThrottleUI :: ThrUIParams ParamReader -> Task p ThrottleUI
taskThrottleUI params = do
  uniq <- fresh
  alt_setpoint <- taskLocal "alt_setpoint"
  vel_setpoint <- taskLocal "vel_setpoint"
  let proc_update :: Def('[ FlightMode
                          , ArmedMode
                          , Ref s (Struct "userinput_result")
                          , IFloat
                          , IFloat
                          ] :-> ())
      proc_update  = proc ("throttle_ui_update" ++ show uniq) $
        \fm a ui alt vz -> body $ do
          sens <- paramRead (thrUIsens params)
          dead <- paramRead (thrUIdead params)
          thr  <- deref (ui ~> UI.throttle)
          -- XXX find james's code from deprecated controller for here.
          return ()
  taskModuleDef $ incl proc_update
  return ThrottleUI
    { ui_update   = call_ proc_update
    , ui_setpoint = do
        a <- deref alt_setpoint
        v <- deref vel_setpoint
        return (a,v)
    , ui_write_debug = \_dbg -> return () -- XXX
    }

