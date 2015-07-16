{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.IO.RCInput
  ( rcInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C ()
import qualified SMACCMPilot.Comm.Ivory.Types.RcInput as RC

import SMACCMPilot.Flight.IO.RCInput.Decode

rcInputTower :: ChanOutput (Struct "rc_input")
              -> ChanInput (Struct "user_input")
              -> ChanInput (Struct "control_law")
              -> Tower e ()
rcInputTower rc ui cl = do
  p <- period (Milliseconds 50)

  monitor "rcin_userinput_translator" $ do
    rcin       <- state "rcin"
    valid      <- state "valid"
    decoder    <- monitorRCInputDecoder

    handler systemInit "userinput_init" $ callback $ const $ do
      rcind_init decoder
      store valid false

    handler rc "rcin_userinput_capt" $ do
      callback $ \rc_in -> do
        refCopy rcin rc_in
        refCopy valid (rc_in ~> RC.valid)

    handler p "periodic_userinput_decode" $ do
      -- Invariant: multiplexer depends on message cl being delivered before ui.
      -- Tower should deliver messages in order of the emitter declarations.
      -- cl and ui are emitted on every period tick.
      cl_emitter <- emitter cl 1
      ui_emitter <- emitter ui 1
      callbackV $ \now -> do
        v <- deref valid
        ifte_ v
              (rcind_new_sample decoder (constRef rcin))
              (rcind_no_sample decoder now)
        rcind_get_ui     decoder >>= emit ui_emitter
        rcind_get_cl_req decoder >>= emit cl_emitter
        store valid false

