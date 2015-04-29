{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.PPM
  ( ppmInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C ()

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.UserInput.PPM.Decode

import SMACCMPilot.Hardware.PPM

ppmInputTower :: (e -> PPM)
              -> (e -> ClockConfig)
              -> Tower e ( ChanOutput (Struct "user_input")
                         , ChanOutput (Struct "control_law"))
ppmInputTower toppm tocc = do
  ui <- channel
  cl <- channel
  p <- period (Milliseconds 50)

  ppm <- ppmTower toppm tocc

  monitor "ppm_userinput_decode" $ do
    ppms       <- state "ppms"
    valid      <- state "valid"
    decoder    <- monitorPPMDecoder

    handler systemInit "userinput_init" $ callback $ const $ do
      ppmd_init decoder
      store valid false

    handler ppm "ppm_userinput_capt" $ do
      callback $ \ppms' -> do
        refCopy ppms ppms'
        store   valid true

    handler p "periodic_userinput_decode" $ do
      -- Invariant: multiplexer depends on message cl being delivered before ui.
      -- Tower should deliver messages in order of the emitter declarations.
      -- cl and ui are emitted on every period tick.
      cl_emitter <- emitter (fst cl) 1
      ui_emitter <- emitter (fst ui) 1
      callbackV $ \now -> do
        v <- deref valid
        ifte_ v
              (ppmd_new_sample decoder ppms now)
              (ppmd_no_sample decoder now)
        ppmd_get_ui     decoder >>= emit ui_emitter
        ppmd_get_cl_req decoder >>= emit cl_emitter
        store valid false

  return (snd ui, snd cl)

