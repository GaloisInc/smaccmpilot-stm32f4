{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- UARTTest.hs --- Test STM32F4 UART Driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Monad (zipWithM_)
import Data.Char (ord)

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend
import Ivory.HW

import qualified Ivory.HW.SearchDir as HW
import Ivory.HW.Module (hw_moduledef)
import Ivory.BSP.STM32F4.GPIO

import qualified Ivory.BSP.STM32F4.Interrupt as ISR
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Peripheral

ledPins :: [GPIOPin]
ledPins = [pinB14, pinB15]

ledSetupPin :: GPIOPin -> Ivory eff ()
ledSetupPin pin = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
  pinSetMode pin gpio_mode_analog

ledOn :: GPIOPin -> Ivory eff ()
ledOn pin = do
  pinClear pin
  pinSetMode pin gpio_mode_output

ledOff :: GPIOPin -> Ivory eff ()
ledOff pin = pinSetMode pin gpio_mode_analog

main_task :: Def ('[Ptr s (Stored Uint8)] :-> ())
main_task = proc "main_task" $ \_ -> body $ do
  uartInit uart1 115200
  mapM_ ledSetupPin ledPins
  forever $ do
    call_ taskDelay 250
    setDR uart1 (fromIntegral (ord '0'))
    zipWithM_ ($) (cycle [ledOn, ledOff]) ledPins

    call_ taskDelay 250
    setDR uart1 (fromIntegral (ord '1'))
    zipWithM_ ($) (cycle [ledOff, ledOn]) ledPins

taskDelay :: Def ('[Uint32] :-> ())
taskDelay = importProc "vTaskDelay" "FreeRTOS.h"

blinkModule :: Module
blinkModule = package "ledblink" $ do
  incl main_task
  inclHeader "task.h"
  incl taskDelay
  hw_moduledef

main :: IO ()
main = compileWith Nothing (Just [BSP.searchDir, HW.searchDir]) [blinkModule]

