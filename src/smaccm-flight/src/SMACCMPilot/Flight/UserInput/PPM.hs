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

import SMACCMPilot.Flight.UserInput.PPM.Decode

ppmInputTower :: Tower e ( ChanOutput (Struct "user_input")
                         , ChanOutput (Struct "control_law"))
ppmInputTower = do
  ui <- channel
  cl <- channel
  p <- period (Milliseconds 50)
  monitor "ppm_decode" $ do
    decoder    <- monitorPPMDecoder

    handler systemInit "userinput_init" $ callback $ const $
      ppmd_init decoder

    handler p "periodic_userinput_decode" $ do
      ui_emitter <- emitter (fst ui) 1
      cl_emitter <- emitter (fst cl) 1
      callbackV $ \now -> do
        chs <- local izero
        captured <- assign (0 :: Uint32) -- call userPPMInputCapture chs
        ifte_ (captured >=? num_chans)
              (ppmd_new_sample decoder chs now)
              (ppmd_no_sample decoder now)
        ppmd_get_ui     decoder >>= emit ui_emitter
        ppmd_get_cl_req decoder >>= emit cl_emitter

  return (snd ui, snd cl)
  where
  num_chans = 6

