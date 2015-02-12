{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.UserInput.PPM.ArmingMachine
  ( ArmingMachine(..)
  , taskArmingMachine
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CL

data ArmingMachine =
  ArmingMachine
    { am_init       :: forall eff   . Ivory eff ()
    , am_new_sample :: forall eff s . Ref s I.PPMs -> ITime -> Ivory eff ()
    , am_no_sample  :: forall eff   . Ivory eff ()
    , am_get_cl_req :: forall eff s . Ref s (Struct "control_law_request")
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

taskArmingMachine :: Task p ArmingMachine
taskArmingMachine = do
  fr <- fresh
  arming_state      <- taskLocal "arming_state"
  arming_state_time <- taskLocal "arming_state_time"
  dead_last_pos     <- taskLocal "dead_last_pos"
  let named n = "ppmdecoder_arming_" ++ n ++ "_" ++ show fr

      init_proc :: Def('[]:->())
      init_proc = proc (named "init") $ body $ do
        store arming_state armingIdle
        store arming_state_time 0
        store dead_last_pos deadSafe

      new_sample_proc :: Def('[Ref s I.PPMs, ITime]:->())
      new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
        throttle_chan   <- deref (ppms ! (2 :: Ix 8))
        rudder_chan     <- deref (ppms ! (3 :: Ix 8))
        dead_chan       <- deref (ppms ! (5 :: Ix 8))
        dead_pos <- assign $ ((dead_chan >? 1600) ? (deadArmable,deadSafe))
        store dead_last_pos dead_pos
        ifte_ (dead_pos ==? deadSafe)
              (arming_reset time)
              (arming_sm throttle_chan rudder_chan time)


      arming_reset :: ITime -> Ivory eff ()
      arming_reset time = do
        store arming_state armingIdle
        store arming_state_time time

      arming_sm :: I.PPM -> I.PPM -> ITime -> Ivory eff ()
      arming_sm thr rud time = do
        prevt <- deref arming_state_time
        state <- deref arming_state
        sticks_corner <- assign $ thr <? 1050 .&& rud >? 1900
        donewaiting   <- assign $ (time - prevt) >? hystresis
        cond_
          [ state ==? armingIdle .&& sticks_corner ==> do
              store arming_state armingActive
              store arming_state_time time
          , state ==? armingActive .&& sticks_corner ==> do
              when donewaiting $ store arming_state armingComplete
          , true ==> do
              store arming_state armingIdle
          ]
        where
        hystresis = 750

      no_sample_proc :: Def('[] :-> ())
      no_sample_proc = proc (named "no_sample") $ body $
        store dead_last_pos deadSafe

      get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
      get_cl_req_proc = proc (named "cl_req_proc") $ \cl_req -> body $ do
        d <- deref dead_last_pos
        store (cl_req ~> CL.set_safe) (d ==? deadSafe)
        s <- deref arming_state
        when (s ==? armingComplete) $ do
          store (cl_req ~> CL.set_armed) true


  taskModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_cl_req_proc

  return ArmingMachine
    { am_init       = call_ init_proc
    , am_new_sample = call_ new_sample_proc
    , am_no_sample  = call_ no_sample_proc
    , am_get_cl_req = call_ get_cl_req_proc
    }
