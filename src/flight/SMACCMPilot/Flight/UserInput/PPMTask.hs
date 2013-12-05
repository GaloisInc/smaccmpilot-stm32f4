{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Process PPM signals from the PPM RC controller.

module SMACCMPilot.Flight.UserInput.PPMTask
  ( userPPMInputTower
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import SMACCMPilot.Flight.Types.UserInput
import SMACCMPilot.Flight.Types.UserInputSource
import SMACCMPilot.Flight.UserInput.Decode

--------------------------------------------------------------------------------

userPPMInputTower :: Tower p (DataSink (Struct "userinput_result"), DataSink PPMs)
userPPMInputTower = do
  userinput <- dataport
  ppm_chans <- dataport
  task "userPPMInput" $ userPPMInputTask (src userinput) (src ppm_chans)
  return (snk userinput, snk ppm_chans)

userPPMInputTask :: -- To reading Mux
                    DataSource (Struct "userinput_result")
                    -- To armed mux
                 -> DataSource PPMs
                 -> Task p ()
userPPMInputTask uis ppm = do
  uiWriter       <- withDataWriter uis "userInput"
  ppmChansWriter <- withDataWriter ppm "ppmChansWriter"

  chs        <- taskLocal "channels"
  ui_result  <- taskLocal "userinput"

  onPeriod 50 $ \now -> do
    captured <- call userPPMInputCapture chs
    when (captured >=? 6) $ do
      let cchs = constRef chs
      validPPMs <- call userInputFilter cchs
      when validPPMs $ do
        -- Only send valid PPM signals to the arming mux, and only try to decode
        -- valid signals.
        writeData ppmChansWriter cchs
        b <- call deadManSwitch cchs
        when b (call_ userInputDecode chs ui_result now)
    -- Even if the signals are invalid, we're going to call the fail-safe which
    -- zeros the values after 150 ms.
    call_ userInputFailsafe ui_result now
    store (ui_result ~> source) uiSourcePPM
    writeData uiWriter (constRef ui_result)

  taskModuleDef $ do
    depend userInputTypeModule
    depend userInputDecodeModule
    inclHeader "apwrapper/userinput_capture.h"
    incl userPPMInputCapture

--------------------------------------------------------------------------------

-- This talks to the AP_HAL via c++, so we have to extern it completely
userPPMInputCapture :: Def ('[ Ref s1 PPMs ] :-> Uint8)
userPPMInputCapture = externProc "userinput_capture"

--------------------------------------------------------------------------------

