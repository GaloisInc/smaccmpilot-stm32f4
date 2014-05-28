{-# LANGUAGE TemplateHaskell #-}
--
-- Pins.hs -- definition of GPIO pins
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.GPIO.Pins where


import Ivory.BSP.STM32.Peripheral.GPIOF4.Regs
import Ivory.BSP.STM32.Peripheral.GPIOF4.TH

import Ivory.BSP.STM32F405.GPIO.Ports

-- Each mkGPIOPins creates 16 pin values (with 0..15) using TH.
-- so, pins will have values pinA0, pinA1 ... pinA15, etc.

mkGPIOPins 'gpioA "pinA"
mkGPIOPins 'gpioB "pinB"
mkGPIOPins 'gpioC "pinC"
mkGPIOPins 'gpioD "pinD"
mkGPIOPins 'gpioE "pinE"
mkGPIOPins 'gpioF "pinF"
mkGPIOPins 'gpioG "pinG"
mkGPIOPins 'gpioH "pinH"
mkGPIOPins 'gpioI "pinI"
