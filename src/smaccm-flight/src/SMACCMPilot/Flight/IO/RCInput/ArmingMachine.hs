{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.IO.RCInput.ArmingMachine
  ( ArmingMachine(..)
  , monitorArmingMachine
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate    as T
import qualified SMACCMPilot.Comm.Ivory.Types.RcInput     as RC

data ArmingMachine =
  ArmingMachine
    { am_init       :: forall eff   . Ivory eff ()
    , am_new_sample :: forall eff s . ConstRef s ('Struct "rc_input") -> Ivory eff ()
    , am_no_sample  :: forall eff   . Ivory eff ()
    , am_get_req    :: forall eff s . Ref s ('Stored T.Tristate)
                                   -> Ivory eff ()
    }

newtype DeadSwitch = DeadSwitch Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

deadSafe :: DeadSwitch
deadSafe = DeadSwitch 0
deadArmable :: DeadSwitch
deadArmable = DeadSwitch 1

newtype ArmingState = ArmingState Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

armingIdle :: ArmingState
armingIdle = ArmingState 0
armingActive :: ArmingState
armingActive = ArmingState 1
armingComplete :: ArmingState
armingComplete = ArmingState 2

monitorArmingMachine :: Monitor p ArmingMachine
monitorArmingMachine = do
  arming_state      <- state "arming_state"
  arming_state_time <- state "arming_state_time"
  arming_event      <- state "arming_event"
  dead_last_pos     <- state "dead_last_pos"
  let named n = fmap showUnique $ freshname $ "ppmdecoder_arming_" ++ n

  init_name <- named "init"
  new_sample_name <- named "new_sample"
  no_sample_name <- named "no_sample"
  get_arming_mode_name <- named "get_arming_mode"

  let init_proc :: Def('[]':->())
      init_proc = proc init_name $ body $ do
        store arming_state armingIdle
        store arming_state_time 0
        store arming_event false
        store dead_last_pos deadSafe

      new_sample_proc :: Def('[ConstRef s ('Struct "rc_input")] ':-> ())
      new_sample_proc = proc new_sample_name $ \rcin -> body $ do
        throttle_chan   <- deref (rcin ~> RC.throttle)
        rudder_chan     <- deref (rcin ~> RC.yaw)
        dead_chan       <- deref (rcin ~> RC.switch2)
        time            <- fmap iTimeFromTimeMicros (deref (rcin ~> RC.time))
        dead_pos <- assign $ ((dead_chan >? 1600) ? (deadArmable,deadSafe))
        store dead_last_pos dead_pos
        ifte_ (dead_pos ==? deadSafe)
              (arming_reset time >> store arming_event false)
              (arming_sm throttle_chan rudder_chan time)


      arming_reset :: ITime -> Ivory eff ()
      arming_reset time = do
        store arming_state armingIdle
        store arming_state_time time

      arming_sm :: Uint16 -> Uint16 -> ITime -> Ivory eff ()
      arming_sm thr rud time = do
        prevt <- deref arming_state_time
        s <- deref arming_state
        sticks_corner <- assign $ thr <? 1050 .&& rud >? 1900
        donewaiting   <- assign $ (time - prevt) >? hystresis
        cond_
          [ s ==? armingIdle .&& sticks_corner ==> do
              store arming_state armingActive
              store arming_state_time time
          , s ==? armingActive .&& sticks_corner ==> do
              when donewaiting $ do
                store arming_state armingComplete
                store arming_event true
          , true ==> do
              store arming_state armingIdle
          ]
        where
        hystresis = 750

      no_sample_proc :: Def('[] ':-> ())
      no_sample_proc = proc no_sample_name $ body $
        store dead_last_pos deadSafe

      get_arming_mode_proc :: Def('[Ref s ('Stored T.Tristate)]':->())
      get_arming_mode_proc = proc get_arming_mode_name $ \a -> body $ do
        d <- deref dead_last_pos
        e <- deref arming_event
        store arming_event false
        cond_
          [(d ==? deadSafe) ==> store a T.negative
          ,(e ==? false)    ==> store a T.neutral
          ,(e ==? true)     ==> store a T.positive
          ]


  monitorModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_arming_mode_proc

  return ArmingMachine
    { am_init       = call_ init_proc
    , am_new_sample = call_ new_sample_proc
    , am_no_sample  = call_ no_sample_proc
    , am_get_req = call_ get_arming_mode_proc
    }
