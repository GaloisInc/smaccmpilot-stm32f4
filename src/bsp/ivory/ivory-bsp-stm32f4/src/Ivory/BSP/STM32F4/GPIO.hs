--
-- GPIO.hs --- GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GPIO (
    GPIOPort()
  , gpioA, gpioB, gpioC, gpioD, gpioE, gpioF
  , gpioG, gpioH, gpioI

  , GPIOPin(), pinEnable, pinSetMode, pinSetOutputType
  , pinSetSpeed, pinSetPUPD, pinSetAF
  , pinSet, pinClear

  , module Ivory.BSP.STM32F4.GPIO.Pins
  , module Ivory.BSP.STM32F4.GPIO.RegTypes
) where

import Ivory.BSP.STM32F4.GPIO.RegTypes
import Ivory.BSP.STM32F4.GPIO.Types
import Ivory.BSP.STM32F4.GPIO.Pins
