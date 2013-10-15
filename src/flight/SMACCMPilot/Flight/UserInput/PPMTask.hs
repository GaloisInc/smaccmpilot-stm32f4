{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Process PPM signals from the PPM RC controller.

module SMACCMPilot.Flight.UserInput.PPMTask
  ( userPPMInputTask
  , userPPMInputCapture
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import SMACCMPilot.Flight.Types.UserInput
import SMACCMPilot.Flight.UserInput.Decode

--------------------------------------------------------------------------------

userPPMInputTask :: -- To reading Mux
                    DataSource (Struct "userinput_result")
                    -- To control task
                 -> DataSource (Struct "flightmode")
                    -- To armed mux
                 -> DataSource (Array 8 (Stored Uint16))
                 -> Task p ()
userPPMInputTask uis fms ppm = do
  fmWriter       <- withDataWriter fms "flightMode"
  uiWriter       <- withDataWriter uis "userInput"
  ppmChansWriter <- withDataWriter ppm "ppmChansWriter"

  chs        <- taskLocal "channels"
  -- decoder    <- taskLocal "decoder"
  ui_result  <- taskLocal "userinput"
  fm_result  <- taskLocal "flightmode"
  armed_res  <- taskLocal "armed"

  onPeriod 50 $ \now -> do
    captured <- call userPPMInputCapture chs
    when captured $ do
      writeData ppmChansWriter (constRef chs)

      call_ userInputDecode chs ui_result now
      -- call_ setFlightMode chs decoder fm_result now --XXX
    call_ userInputFailsafe ui_result fm_result armed_res now
    writeData uiWriter (constRef ui_result)
    writeData fmWriter (constRef fm_result)

  taskModuleDef $ do
    depend userInputTypeModule
    depend userInputDecodeModule
    inclHeader "apwrapper/userinput_capture.h"
    incl userPPMInputCapture

--------------------------------------------------------------------------------

-- This talks to the AP_HAL via c++, so we have to extern it completely
userPPMInputCapture :: Def ('[ Ref s1 (Array 8 (Stored Uint16)) ] :-> IBool)
userPPMInputCapture = externProc "userinput_capture"

--------------------------------------------------------------------------------

