{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleTracker where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ArmedMode    as A
import qualified SMACCMPilot.Flight.Types.ControlLaw   as CL
import qualified SMACCMPilot.Flight.Types.ThrottleMode as TM
import qualified SMACCMPilot.Flight.Types.UserInput    as UI

data ThrottleTracker =
  ThrottleTracker
    { tt_init      :: forall eff   . Ivory eff ()
    , tt_update    :: forall eff s s' .  Ref s  (Struct "userinput_result")
                                      -> Ref s' (Struct "control_law")
                                      -> Ivory eff ()
    , tt_reset_to  :: forall eff   . Ivory eff (IBool, IFloat)
    }

taskThrottleTracker :: Task p ThrottleTracker
taskThrottleTracker = do
  uniq <- fresh
  last_thr_mode  <- taskLocal "last_tm"
  last_throttle  <- taskLocal "last_throttle"
  reset_required <- taskLocal "reset_required"
  let proc_update :: Def('[ Ref s  (Struct "userinput_result")
                          , Ref s' (Struct "control_law")
                          ] :->())
      proc_update = proc ("throttle_tracker_update_" ++ show uniq) $
        \ui cl -> body $ do
          l_tm  <- deref last_thr_mode
          tm    <- deref (cl ~> CL.thr_mode)
          armed <- deref (cl ~> CL.armed_mode)
          store last_thr_mode tm
          cond_
            [   l_tm  ==? TM.direct
            .&& tm    /=? TM.autothrottle
            .&& armed ==? A.armed
              ==> store reset_required true
            ,   tm    ==? TM.direct
            .&& armed ==? A.armed
              ==> manual_throttle ui >>= store last_throttle
            ]
  taskModuleDef $ incl proc_update
  return ThrottleTracker
    { tt_init = do
        store last_thr_mode  TM.direct
        store last_throttle  (0 :: IFloat)
        store reset_required true
    , tt_update = call_ proc_update
    , tt_reset_to = do
        rr <- deref reset_required
        lt <- deref last_throttle
        store reset_required false
        return (rr,lt)
    }

manual_throttle :: Ref s (Struct "userinput_result") -> Ivory eff IFloat
manual_throttle ui = do
  thr <- deref (ui ~> UI.throttle)
  -- -1 =< UI.thr =< 1.  Scale to 0 =< thr' =< 1.
  return ((thr + 1) / 2)

