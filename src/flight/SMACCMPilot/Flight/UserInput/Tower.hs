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

--------------------------------------------------------------------------------

userInputTower :: (SingI n0, SingI n1)
                  -- Mux'ed armed
               => ( DataSource (Stored A.ArmedMode)
                  , DataSink (Stored A.ArmedMode))
                  -- MAVLink armed
               -> ChannelSink n0 (Stored A.ArmedMode)
                  -- From GCS Rx Task
               -> ChannelSink n1 (Struct "rc_channels_override_msg")
               -> Tower p (DataSink (Struct "userinput_result"))
userInputTower armed_res snk_mav_armed snk_rc_over = do
  (src_userinput, snk_userinput)         <- dataport
  (src_rc_over_res, snk_rc_over_res)     <- dataport
  (src_flightmode, _)                    <- dataport
  (src_ppm_chans, snk_ppm_chans)         <- dataport
  (src_input_mux_res, snk_input_mux_res) <- dataport

  -- Handler for PPM Radio messages
  task "userPPMInput" $ userPPMInputTask src_userinput
                                         src_flightmode
                                         src_ppm_chans

  -- Handler for RC override MAVLink messages.
  task "userMAVInput" $ userMAVInputTask (snk armed_res)
                                         snk_rc_over
                                         src_rc_over_res

  task "armedMux"     $ armedMuxTask snk_ppm_chans
                                     snk_mav_armed
                                     (src armed_res)

  task "userInputMux" $ userInputMuxTask (snk armed_res)
                                         snk_userinput
                                         snk_rc_over_res
                                         src_input_mux_res

  addModule userInputDecodeModule
  return snk_input_mux_res

--------------------------------------------------------------------------------
