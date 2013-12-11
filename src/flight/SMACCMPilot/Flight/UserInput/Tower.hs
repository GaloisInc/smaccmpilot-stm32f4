{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Tower
  ( userInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Flight.UserInput.PPMTask
import           SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import           SMACCMPilot.Flight.UserInput.RCOverride
import           SMACCMPilot.Flight.UserInput.Mux

import           SMACCMPilot.Mavlink.Messages.RcChannelsOverride
--------------------------------------------------------------------------------

userInputTower :: (SingI n0, SingI n1)
               => ChannelSink n0 (Struct "control_request")
               -> ChannelSink n1 (Struct "rc_channels_override_msg")
               -> Tower p ( DataSink (Struct "userinput_result")
                          , DataSink (Struct "control_law")
                          )
userInputTower snk_ctl_req snk_rc_over = do
  armed_state   <- dataport

  (ppm_ui, ppm_chans) <- userPPMInputTower

  (rcoverride_ui, rcoverride_active) <- userRCOverrideTower
                                          snk_rc_over
                                          (snk armed_state)

  task "armingTask" $ armedMuxTask ppm_chans
                                   snk_ctl_req
                                   (src armed_state)
  controllaw <- controlLawMuxTower
                  ppm_chans
                  (snk armed_state)

  canonical_ui <- userInputMuxTower
                       ppm_ui
                       rcoverride_ui
                       rcoverride_active
                       controllaw

  addDepends rcChannelsOverrideModule
  addModule userInputDecodeModule
  return (canonical_ui, controllaw)

--------------------------------------------------------------------------------
