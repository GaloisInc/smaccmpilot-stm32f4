{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
    { am_init       :: forall eff . Ivory eff ()
    , am_new_sample :: forall eff s . Ref s I.PPMs -> Uint32 -> Ivory eff ()
    , am_no_sample  :: forall eff . Uint32 -> Ivory eff ()
    , am_get_cl_req :: forall eff s . Ref s (Struct "control_law_request")
                                   -> Ivory eff ()
    }


newtype DeadSwitch = DeadSwitch Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

deadSafe :: DeadSwitch
deadSafe = DeadSwitch 0
deadArmable :: DeadSwitch
deadArmable = DeadSwitch 1

taskArmingMachine :: Task p ArmingMachine
taskArmingMachine = do
  fr <- fresh
  dead_last_pos <- taskLocal "dead_last_pos"
  last_time <- taskLocal "last_time"
  let named n = "ppmdecoder_arming_" ++ n ++ "_" ++ show fr

      init_proc :: Def('[]:->())
      init_proc = proc (named "init") $ body $ do
        store dead_last_pos deadSafe
        store last_time 0

      new_sample_proc :: Def('[Ref s I.PPMs, Uint32]:->())
      new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
        dead_chan <- deref (ppms ! (5 :: Ix 8))
        dead_pos <- assign $ ((dead_chan >? 1500) ? (deadSafe,deadArmable))
        store dead_last_pos dead_pos
        store last_time time
        -- XXX arming machine

      no_sample_proc :: Def('[Uint32]:->())
      no_sample_proc = proc (named "no_sample") $ \time -> body $ do
        store dead_last_pos deadSafe

      get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
      get_cl_req_proc = proc (named "cl_req_proc") $ \cl_req -> body $ do
        d <- deref dead_last_pos
        store (cl_req ~> CL.set_safe) (d ==? deadSafe)
        -- XXX arming machine

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
  where
  timeout = 150
