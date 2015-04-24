{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.PPM
  ( ppmInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import           SMACCMPilot.Flight.Types.ControlLawRequest ()

import SMACCMPilot.Flight.UserInput.PPM.Decode

ppmInputTower :: Tower p ( ChannelSink (Struct "userinput_result")
                         , ChannelSink (Struct "control_law_request"))
ppmInputTower = do
  ui <- channel
  cr <- channel
  task "ppmInputTower" $ do
    ui_emitter <- withChannelEmitter (src ui) "ui_emitter"
    cr_emitter <- withChannelEmitter (src cr) "cr_emitter"
    decoder    <- taskPPMDecoder

    taskInit $ do
      ppmd_init decoder

    onPeriod (Milliseconds 50) $ \now -> do
      chs <- local izero
      captured <- call userPPMInputCapture chs
      ifte_ (captured >=? num_chans)
            (ppmd_new_sample decoder chs now)
            (ppmd_no_sample decoder now)
      ppmd_get_ui     decoder >>= emit_ ui_emitter
      ppmd_get_cl_req decoder >>= emit_ cr_emitter

    taskModuleDef $ do
      inclHeader "apwrapper/userinput_capture.h"
      incl userPPMInputCapture

  return (snk ui, snk cr)
  where
  num_chans = 6

-- This talks to the AP_HAL via c++, so we have to extern it completely
userPPMInputCapture :: Def ('[ Ref s I.PPMs ] :-> Uint8)
userPPMInputCapture = externProc "userinput_capture"

