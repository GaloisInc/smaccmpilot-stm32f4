{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleTracker where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput    as UI

data ThrottleTracker =
  ThrottleTracker
    { tt_init      :: forall eff   . Ivory eff ()
    , tt_update    :: forall eff s . Ref s  (Struct "user_input")
                                  -> IBool
                                  -> Ivory eff ()
    , tt_reset_to  :: forall eff   . Ivory eff (IBool, IFloat)
    }

monitorThrottleTracker :: Monitor p ThrottleTracker
monitorThrottleTracker = do
  last_en        <- state "last_en"
  last_throttle  <- state "last_throttle"
  reset_required <- state "reset_required"
  name_update <- fmap showUnique $ freshname "throttle_tracker_update"
  let proc_update :: Def('[ Ref s  (Struct "user_input")
                          , IBool
                          ] :->())
      proc_update = proc name_update $
        \ui enabled -> body $ do
          prev_enabled <- deref last_en
          store last_en enabled
          cond_
            [ enabled .&& iNot prev_enabled
                ==> store reset_required true
            , iNot enabled
                ==> manual_throttle ui >>= store last_throttle
            ]

  monitorModuleDef $ incl proc_update
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

manual_throttle :: Ref s (Struct "user_input") -> Ivory eff IFloat
manual_throttle ui = do
  thr <- deref (ui ~> UI.throttle)
  -- -1 =< UI.thr =< 1.  Scale to 0 =< thr' =< 1.
  return ((thr + 1) / 2)

