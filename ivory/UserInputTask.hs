{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module UserInputTask where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS

import UserInputType
import UserInputDecode

import IvoryHelpers

userInputTask :: Source (Struct "userinput_result")
          -> String -> Task
userInputTask s uniquename =
  withSource "userInputSource" s $ \userInputSource ->
  let tDef = proc ("userInputTaskDef" ++ uniquename) $ body $ do

        initTime <- call OS.getTimeMillis
        lastTime <- local (ival initTime)

        chs     <- local (iarray [])
        decoder <- local (istruct [])
        result  <- local (istruct [])

        forever $ do
          captured <- call userInputCapture chs
          ift captured $ do
            now <- call OS.getTimeMillis 
            call_ decode chs decoder result now
          call_ userInputFailsafe result
          source userInputSource (constRef result)
          call_ OS.delayUntil lastTime period

      mName = "userinputTask" ++ uniquename
      mDefs = do
        depend userInputTypeModule
        depend userInputDecodeModule
        depend OS.taskModule
        inclHeader "userinput_capture"
        incl tDef
        incl userInputCapture
        private $ do
          incl userInputFailsafe

  in task tDef mName mDefs
  where
  period = 50

-- This talks to the AP_HAL via c++, so we have to extern it completely
userInputCapture :: Def ('[ Ref s1 (Array 8 (Stored Uint16)) ] :-> IBool)
userInputCapture = externProc "userinput_capture"

userInputFailsafe :: Def ('[ Ref s1 (Struct "userinput_result") ] :-> ())
userInputFailsafe = proc "userInputFailsafe" $ \capt -> body $ do
  last <- deref ( capt ~> time )
  now  <- call OS.getTimeMillis
  let dt = now - last
  ift (dt >? 150) $ do
     store (capt ~> armed)    false
     store (capt ~> throttle) 0
     store (capt ~> yaw)      0
     store (capt ~> pitch)    0
     store (capt ~> roll)     0
  retVoid
