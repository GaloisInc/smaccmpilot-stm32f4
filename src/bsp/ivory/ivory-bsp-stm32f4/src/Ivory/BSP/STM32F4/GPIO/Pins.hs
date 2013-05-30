{-# LANGUAGE TemplateHaskell #-}
--
-- Pins.hs --- GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GPIO.Pins where

import Language.Haskell.TH

import Ivory.BSP.STM32F4.GPIO.Regs
import Ivory.BSP.STM32F4.GPIO.TH
import Ivory.BSP.STM32F4.GPIO.Types

mkGPIOPins 'gpioA "pinA"
mkGPIOPins 'gpioB "pinB"
mkGPIOPins 'gpioC "pinC"
mkGPIOPins 'gpioD "pinD"
mkGPIOPins 'gpioE "pinE"
mkGPIOPins 'gpioF "pinF"
mkGPIOPins 'gpioG "pinG"
mkGPIOPins 'gpioH "pinH"
mkGPIOPins 'gpioI "pinI"
