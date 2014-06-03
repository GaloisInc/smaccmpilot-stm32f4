--
-- GPIO.hs --- GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.GPIO
  ( GPIOPort()
  , GPIOPin(), pinEnable, pinSetMode, pinSetOutputType
  , pinSetSpeed, pinSetPUPD, pinSetAF
  , pinSet, pinClear
  , pinRead

  , module Ivory.BSP.STM32F405.GPIO.Pins
  , module Ivory.BSP.STM32F405.GPIO.Ports
  , module Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes
  , GPIO_AF()
  ) where

import Ivory.BSP.STM32F405.GPIO.Pins
import Ivory.BSP.STM32F405.GPIO.Ports

import Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes
import Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral
