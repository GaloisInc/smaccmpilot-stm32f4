--
-- API.hs --- UART Peripheral Description
-- UART Peripheral public interface
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.UART.API where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.UART.Regs
import Ivory.BSP.STM32F4.UART.Peripheral

import           Ivory.BSP.STM32F4.GPIO
import qualified Ivory.BSP.STM32F4.GPIO.AF as GPIO

initPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initPin p af = do
  pinEnable        p
  pinSetSpeed      p gpio_speed_50mhz
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetPUPD       p gpio_pupd_pullup
  pinSetAF         p af


