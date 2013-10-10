{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Tower
  ( userInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.UserInput.PPMTask
import SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import SMACCMPilot.Flight.UserInput.RCOverride
import SMACCMPilot.Flight.UserInput.Mux

--------------------------------------------------------------------------------

userInputTower :: -- From GCS Rx Task
                  DataSink (Struct "rc_channels_override_msg")
               -> Tower p ( DataSink (Struct "userinput_result")
                          , DataSink (Struct "flightmode")
                          )
userInputTower snk_rc_over = do
  (src_userinput, snk_userinput)         <- dataport
  (src_rc_over_res, snk_rc_over_res)     <- dataport
  (src_flightmode, snk_flightmode)       <- dataport
  (src_input_mux_res, snk_input_mux_res) <- dataport

  -- Handler for PPM Radio messages
  task "userPPMInput" $ userPPMInputTask src_userinput src_flightmode
  -- Handler for RC override MAVLink messages.
  task "userMAVInput" $ userMAVInputTask snk_rc_over src_rc_over_res
  task "userInputMux" $ userInputMuxTask snk_userinput
                                         snk_rc_over_res
                                         src_input_mux_res

  addModule userInputDecodeModule
  return (snk_input_mux_res, snk_flightmode)

--------------------------------------------------------------------------------

