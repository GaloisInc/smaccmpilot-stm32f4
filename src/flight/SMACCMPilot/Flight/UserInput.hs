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

userInputTower :: ChannelSink 16 (Struct "control_law_request")
               -> ChannelSink 16 (Struct "rc_channels_override_msg")
               -> Tower p ( ChannelSink 16 (Struct "userinput_result")
                          , ChannelSink 16 (Struct "control_law"))
userInputTower mav_ctl_req _mav_rc_ovr = do
  -- PPM module provides canonical control law request
  (ppm_ui, ppm_cl_req) <- ppmInputTower
  -- Combine PPM and Mavlink control law requests into control law
  cl <- controlLawTower ppm_cl_req mav_ctl_req
  -- Combine PPM user input with rc override, using control law
    -- XXX to implement
  -- Return the canonical user input and the control law
  return (ppm_ui, cl)
