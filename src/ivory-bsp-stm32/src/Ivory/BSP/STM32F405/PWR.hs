--
-- PWR.hs --- Power peripheral driver for STM32F405/407/415/417
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.PWR
  ( module Ivory.BSP.STM32.Peripheral.PWR.RegTypes
  , module Ivory.BSP.STM32.Peripheral.PWR.Regs
  , PWR(..)
  , pwr
  ) where

import Ivory.BSP.STM32.Peripheral.PWR.RegTypes
import Ivory.BSP.STM32.Peripheral.PWR.Regs
import Ivory.BSP.STM32.Peripheral.PWR.Peripheral

import Ivory.BSP.STM32F405.MemoryMap (pwr_periph_base)

pwr :: PWR
pwr = mkPwr pwr_periph_base

