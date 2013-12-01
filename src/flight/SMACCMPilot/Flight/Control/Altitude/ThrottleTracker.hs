{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.ThrottleTracker where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Types.Armed
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import           SMACCMPilot.Flight.Types.FlightModeData

data ThrottleTracker =
  ThrottleTracker
    { tt_init      :: forall eff   . Ivory eff ()
    , tt_update    :: forall eff s . Ref s (Struct "userinput_result")
                                  -> FlightMode -> ArmedMode -> Ivory eff ()
    , tt_reset_to  :: forall eff   . Ivory eff (IBool, IFloat)
    }

-- XXX completely unimplemented
taskThrottleTracker :: Task p ThrottleTracker
taskThrottleTracker = do
  uniq <- fresh
  last_fm        <- taskLocal "last_fm"
  last_throttle  <- taskLocal "last_throttle"
  reset_required <- taskLocal "reset_required"
  let proc_update :: Def('[ Ref s (Struct "userinput_result")
                          , FlightMode
                          , ArmedMode
                          ]:->())
      proc_update = proc ("throttle_tracker_update_" ++ show uniq) $
        \ui fm armed -> body $ do
          l_fm <- deref last_fm
          store last_fm fm
          cond_ 
            [ l_fm ==? flightModeStabilize
                .&& fm /=? flightModeStabilize
                .&& armed ==? as_ARMED
              ==> store reset_required true
            , l_fm ==? flightModeStabilize
                .&& armed ==? as_ARMED
              ==> manual_throttle ui >>= store last_throttle
            ]
  taskModuleDef $ incl proc_update
  return ThrottleTracker
    { tt_init = do
        store last_fm        flightModeStabilize
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

