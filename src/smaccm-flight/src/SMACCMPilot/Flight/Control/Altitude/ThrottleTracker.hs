{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleTracker where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput    as UI

data ThrottleTracker =
  ThrottleTracker
    { tt_init      :: forall eff   . Ivory eff ()
    , tt_update    :: forall eff s . Ref s  (Struct "userinput_result")
                                  -> IBool
                                  -> Ivory eff ()
    , tt_reset_to  :: forall eff   . Ivory eff (IBool, IFloat)
    }

taskThrottleTracker :: Task p ThrottleTracker
taskThrottleTracker = do
  uniq <- fresh
  last_en        <- taskLocal "last_en"
  last_throttle  <- taskLocal "last_throttle"
  reset_required <- taskLocal "reset_required"
  let proc_update :: Def('[ Ref s  (Struct "userinput_result")
                          , IBool
                          ] :->())
      proc_update = proc ("throttle_tracker_update_" ++ show uniq) $
        \ui enabled -> body $ do
          prev_enabled <- deref last_en
          store last_en enabled
          cond_
            [ enabled .&& iNot prev_enabled
                ==> store reset_required true
            , iNot enabled
                ==> manual_throttle ui >>= store last_throttle
            ]

  taskModuleDef $ incl proc_update
  return ThrottleTracker
    { tt_init = do
        store last_en        false
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

