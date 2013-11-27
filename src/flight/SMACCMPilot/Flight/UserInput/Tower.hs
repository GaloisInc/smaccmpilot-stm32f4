{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Tower
  ( userInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.Armed as A
import           SMACCMPilot.Flight.UserInput.PPMTask
import           SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import           SMACCMPilot.Flight.UserInput.RCOverride
import           SMACCMPilot.Flight.UserInput.Mux

import           SMACCMPilot.Mavlink.Messages.RcChannelsOverride
--------------------------------------------------------------------------------

userInputTower :: (SingI n0, SingI n1)
               => ChannelSink n0 (Stored A.ArmedMode)
               -> ChannelSink n1 (Struct "rc_channels_override_msg")
               -> Tower p ( DataSink (Struct "userinput_result")
                          , DataSink (Struct "flightmode")
                          , DataSink (Stored A.ArmedMode)
                          )
userInputTower snk_mav_armed snk_rc_over = do
  armed_state   <- dataport

  (ppm_ui, ppm_chans) <- userPPMInputTower

  (rcoverride_ui, rcoverride_active) <- userRCOverrideTower
                                          snk_rc_over
                                          (snk armed_state)

  task "armingTask" $ armedMuxTask ppm_chans
                                   snk_mav_armed
                                   (src armed_state)

  (canonical_ui) <- userInputMuxTower
                      (snk armed_state)
                       ppm_ui
                       rcoverride_ui
                       rcoverride_active

  flightmode <- flightModeMuxTower
                  ppm_chans
                  (snk armed_state)

  addDepends rcChannelsOverrideModule
  addModule userInputDecodeModule
  return (canonical_ui, flightmode, snk armed_state)

--------------------------------------------------------------------------------
