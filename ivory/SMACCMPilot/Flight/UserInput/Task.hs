{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Task
  ( userInputTask
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS

import SMACCMPilot.Flight.Types.UserInput
import SMACCMPilot.Flight.UserInput.Decode

import SMACCMPilot.Util.IvoryHelpers
import SMACCMPilot.Util.Periodic

userInputTask :: Source (Struct "userinput_result")
          -> String -> Task
userInputTask s uniquename =
  withSource "userInputSource" s $ \userInputSource ->
  let tDef = proc ("userInputTaskDef" ++ uniquename) $ body $ do
        chs     <- local (iarray [])
        decoder <- local (istruct [])
        result  <- local (istruct [])
        periodic 50 $ do
          now <- call OS.getTimeMillis
          captured <- call userInputCapture chs
          ift captured $ do
            call_ userInputDecode chs decoder result now
          call_ userInputFailsafe result now
          source userInputSource (constRef result)

      mDefs = do
        depend userInputTypeModule
        depend userInputDecodeModule
        depend OS.taskModule
        inclHeader "userinput_capture"
        incl tDef
        incl userInputCapture

  in task tDef mDefs

-- This talks to the AP_HAL via c++, so we have to extern it completely
userInputCapture :: Def ('[ Ref s1 (Array 8 (Stored Uint16)) ] :-> IBool)
userInputCapture = externProc "userinput_capture"

