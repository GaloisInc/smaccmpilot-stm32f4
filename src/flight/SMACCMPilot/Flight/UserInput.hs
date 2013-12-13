{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput
  ( userInputTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Types.UserInput  ()
import           SMACCMPilot.Flight.Types.ControlLaw ()

import SMACCMPilot.Flight.UserInput.PPM
import SMACCMPilot.Flight.UserInput.ControlLaw

userInputTower :: Tower p ( ChannelSink 16 (Struct "userinput_result")
                          , ChannelSink 16 (Struct "control_law"))
userInputTower = do
  garbage_cl_req <- channel
  (ppm_ui, ppm_cl_req) <- ppmInputTower
  cl <- controlLawTower ppm_cl_req (snk garbage_cl_req)
  return (ppm_ui, cl)
