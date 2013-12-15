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

import SMACCMPilot.Flight.UserInput.ControlLaw
import SMACCMPilot.Flight.UserInput.PPM
import SMACCMPilot.Flight.UserInput.MAVLink
import SMACCMPilot.Flight.UserInput.Mux

userInputTower :: ChannelSink 16 (Struct "control_law_request")
               -> ChannelSink 16 (Struct "rc_channels_override_msg")
               -> Tower p ( ChannelSink 16 (Struct "userinput_result")
                          , ChannelSink 16 (Struct "control_law"))
userInputTower mav_ctl_req mav_rc_ovr = do
  -- PPM module provides canonical control law request
  (ppm_ui, ppm_cl_req) <- ppmInputTower
  -- Transform mavlink override messages & law requests
  -- into userinput, and control law request that takes into account userinput
  (rcovr_ui, rcovr_ctl_req) <- mavlinkInputTower mav_rc_ovr mav_ctl_req
  -- Combine PPM and Mavlink control law requests into control law
  cl <- controlLawTower ppm_cl_req rcovr_ctl_req
  -- Combine PPM user input with rc override, using control law
  ui <- uiMuxTower cl ppm_ui rcovr_ui
  -- Return the canonical user input and the control law
  return (ui, cl)

