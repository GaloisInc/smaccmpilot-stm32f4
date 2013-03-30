{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Task
  ( userInputTask
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS.Task as Task

import SMACCMPilot.Flight.Types.UserInput
import SMACCMPilot.Flight.UserInput.Decode

import SMACCMPilot.Util.IvoryHelpers
import SMACCMPilot.Util.Periodic

userInputTask :: DataSource (Struct "userinput_result")
              -> DataSource (Struct "flightmode")
              -> String -> Task
userInputTask uis fms uniquename =
  withDataSource "flightMode" fms $ \flightModeSource ->
  withDataSource "userInput"  uis $ \userInputSource ->
  let tDef = proc ("userInputTaskDef" ++ uniquename) $ body $ do
        chs     <- local (iarray [])
        decoder <- local (istruct [])
        ui_result  <- local (istruct [])
        fm_result  <- local (istruct [])
        periodic 50 $ do
          now <- call (direct Task.getTimeMillis)
          captured <- call (direct userInputCapture chs)
          ift captured $ do
            call (direct_ userInputDecode chs decoder ui_result fm_result now)
          call (direct_ userInputFailsafe ui_result fm_result now)
          dataSource userInputSource  (constRef ui_result)
          dataSource flightModeSource (constRef fm_result)

      mDefs = do
        depend userInputTypeModule
        depend userInputDecodeModule
        depend Task.taskModule
        inclHeader "flight-support/userinput_capture"
        incl tDef
        incl userInputCapture

  in task tDef mDefs

-- This talks to the AP_HAL via c++, so we have to extern it completely
userInputCapture :: Def ('[ Ref s1 (Array 8 (Stored Uint16)) ] :-> IBool)
userInputCapture = externProc "userinput_capture"

