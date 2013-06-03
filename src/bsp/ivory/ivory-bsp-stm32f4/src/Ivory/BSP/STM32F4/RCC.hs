--
-- RCC.hs --- RCC (Reset and Clock Control) peripheral driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC (
    RCCDevice(..), rccEnable
  , module Ivory.BSP.STM32F4.RCC.Regs
) where

import Ivory.BSP.STM32F4.RCC.Class
import Ivory.BSP.STM32F4.RCC.Regs
