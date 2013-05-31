{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Task
  ( userInputTask
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Types.UserInput
import SMACCMPilot.Flight.UserInput.Decode

import SMACCMPilot.Util.IvoryHelpers

userInputTask :: DataSource (Struct "userinput_result")
              -> DataSource (Struct "flightmode")
              -> Task ()
userInputTask uis fms = do
  fmWriter <- withDataWriter fms "flightMode"
  uiWriter <- withDataWriter uis "userInput"
  p <- withPeriod 50
  taskBody $ \sch -> do
    chs     <- local (iarray [])
    decoder <- local (istruct [])
    ui_result  <- local (istruct [])
    fm_result  <- local (istruct [])
    eventLoop sch $ onTimer p $ \now -> do
      captured <- call userInputCapture chs
      ift captured $ do
        call_ userInputDecode chs decoder ui_result fm_result now
      call_ userInputFailsafe ui_result fm_result now
      writeData sch uiWriter (constRef ui_result)
      writeData sch fmWriter (constRef fm_result)

  taskModuleDef $ \_sch -> do
    depend userInputTypeModule
    depend userInputDecodeModule
    inclHeader "flight-support/userinput_capture.h"
    incl userInputCapture

-- This talks to the AP_HAL via c++, so we have to extern it completely
userInputCapture :: Def ('[ Ref s1 (Array 8 (Stored Uint16)) ] :-> IBool)
userInputCapture = externProc "userinput_capture"

