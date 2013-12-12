{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.PPM.ModeSwitch
  ( ModeSwitch(..)
  , taskModeSwitch
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CL

data ModeSwitch =
  ModeSwitch
    { ms_init       :: forall eff . Ivory eff ()
    , ms_new_sample :: forall eff s . Ref s I.PPMs -> Uint32 -> Ivory eff ()
    , ms_no_sample  :: forall eff . Uint32 -> Ivory eff ()
    , ms_get_cl_req :: forall eff s . Ref s (Struct "control_law_request")
                                   -> Ivory eff ()
    }

taskModeSwitch :: Task p ModeSwitch
taskModeSwitch = do
  fr <- fresh
  --  md_last_position      <- taskLocal "md_last_position"
  --  md_last_position_time <- taskLocal "md_last_position"
  let named n = "ppmdecoder_modeswitch_" ++ n ++ "_" ++ show fr

      init_proc :: Def('[]:->())
      init_proc = proc (named "init") $ body $ do
        return () -- XXX

      new_sample_proc :: Def('[Ref s I.PPMs, Uint32]:->())
      new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
        return () -- XXX

      no_sample_proc :: Def('[Uint32]:->())
      no_sample_proc = proc (named "no_sample") $ \time -> body $ do
        return () -- XXX

      get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
      get_cl_req_proc = proc (named "cl_req_proc") $ \cl_req -> body $ do
        return () -- XXX

  taskModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_cl_req_proc

  return ModeSwitch
    { ms_init       = call_ init_proc
    , ms_new_sample = call_ new_sample_proc
    , ms_no_sample  = call_ no_sample_proc
    , ms_get_cl_req = call_ get_cl_req_proc
    }
