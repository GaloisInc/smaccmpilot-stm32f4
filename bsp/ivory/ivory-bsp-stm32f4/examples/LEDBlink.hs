{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- LEDBlink.hs --- Blink an LED on the PX4 board.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Monad (zipWithM_)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend
import Ivory.HW
import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO

ledPins :: [GPIOPin]
ledPins = [pinB14, pinB15]

ledSetupPin :: GPIOPin -> Ivory eff ()
ledSetupPin pin = do
  pinEnable pin
  pinSetOutputType pin GPIO_OutputType_PushPull
  pinSetSpeed pin GPIO_Speed_2MHz
  pinSetPUPD pin GPIO_PUPD_None
  pinSetMode pin GPIO_Mode_Analog

ledOn :: GPIOPin -> Ivory eff ()
ledOn pin = do
  pinClear pin
  pinSetMode pin GPIO_Mode_Output

ledOff :: GPIOPin -> Ivory eff ()
ledOff pin = pinSetMode pin GPIO_Mode_Analog

main_task :: Def ('[Ptr s (Stored Uint8)] :-> ())
main_task = proc "main_task" $ \_ -> body $ do
  mapM_ ledSetupPin ledPins
  forever $ do
    call_ taskDelay 250
    zipWithM_ ($) (cycle [ledOn, ledOff]) ledPins
    call_ taskDelay 250
    zipWithM_ ($) (cycle [ledOff, ledOn]) ledPins

cmain :: Def ('[] :-> ())
cmain = proc "main" $ body $ do
  call_ taskCreate (procPtr main_task) "main_task" 1000 nullPtr 0 nullPtr
  call_ taskStartScheduler
  forever $ return ()

----------------------------------------------------------------------
-- FreeRTOS Bindings
--
-- Simple, stand-alone FreeRTOS bindings for this example.

taskCreate :: Def ('[ ProcPtr ('[Ptr s (Stored Uint8)] :-> ())
                    , IString
                    , Uint16
                    , Ptr s (Stored Uint8)
                    , Uint32
                    , Ptr s (Stored Uint8)] :-> Uint32)
taskCreate = importProc "xTaskCreate" "FreeRTOS"

taskStartScheduler :: Def ('[] :-> ())
taskStartScheduler = importProc "vTaskStartScheduler" "FreeRTOS"

taskDelay :: Def ('[Uint32] :-> ())
taskDelay = importProc "vTaskDelay" "FreeRTOS"

cmodule :: Module
cmodule = package "ledblink" $ do
  depend hwModule
  inclHeader "FreeRTOS"
  inclHeader "task"
  incl cmain
  incl main_task
  incl taskCreate
  incl taskStartScheduler

main :: IO ()
main = compile [hwModule, cmodule]
