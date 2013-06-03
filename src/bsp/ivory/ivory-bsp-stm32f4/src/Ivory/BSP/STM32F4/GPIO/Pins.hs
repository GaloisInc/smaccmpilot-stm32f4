{-# LANGUAGE TemplateHaskell #-}
--
-- Pins.hs -- definition of GPIO pins
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GPIO.Pins where

import Language.Haskell.TH

import Ivory.BSP.STM32F4.GPIO.Regs
import Ivory.BSP.STM32F4.GPIO.TH
import Ivory.BSP.STM32F4.GPIO.Peripheral

-- Each mkGPIOPins creates 16 pin values (with 0..15) using TH.

mkGPIOPins 'gpioA "pinA"
mkGPIOPins 'gpioB "pinB"
mkGPIOPins 'gpioC "pinC"
mkGPIOPins 'gpioD "pinD"
mkGPIOPins 'gpioE "pinE"
mkGPIOPins 'gpioF "pinF"
mkGPIOPins 'gpioG "pinG"
mkGPIOPins 'gpioH "pinH"
mkGPIOPins 'gpioI "pinI"
