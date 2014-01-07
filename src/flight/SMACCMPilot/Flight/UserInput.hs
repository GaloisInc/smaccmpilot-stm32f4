{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput
  ( userInputTower
  , UITowerInputs(..)
  ) where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Flight.Types.UserInput  ()
import           SMACCMPilot.Flight.Types.ControlLaw ()

import SMACCMPilot.Flight.UserInput.ControlLaw
import SMACCMPilot.Flight.UserInput.PPM
import SMACCMPilot.Flight.UserInput.MAVLink
import SMACCMPilot.Flight.UserInput.Mux

data UITowerInputs =
  UITowerInputs
    { uit_mavlink_req   :: ChannelSink 16 (Struct "control_law_request")
    , uit_mavlink_rcovr :: ChannelSink 16 (Struct "rc_channels_override_msg")
    , uit_nav_req       :: ChannelSink 16 (Struct "control_law_request")
    }

-- XXX IMPLEMENT USE OF UIT_NAV_REQ
userInputTower :: UITowerInputs
               -> Tower p ( ChannelSink 16 (Struct "userinput_result")
                          , ChannelSink 16 (Struct "control_law"))
userInputTower uit = do
  -- PPM module provides canonical control law request
  (ppm_ui, ppm_cl_req) <- ppmInputTower
  -- Transform mavlink override messages & law requests
  -- into userinput, and control law request that takes into account userinput
  (rcovr_ui, rcovr_ctl_req) <- mavlinkInputTower  (uit_mavlink_rcovr uit)
                                                  (uit_mavlink_req uit)
  -- Combine PPM and Mavlink control law requests into control law
  cl <- controlLawTower ppm_cl_req rcovr_ctl_req (uit_mavlink_req uit)
  -- Combine PPM user input with rc override, using control law
  ui <- uiMuxTower cl ppm_ui rcovr_ui
  -- Return the canonical user input and the control law
  return (ui, cl)

