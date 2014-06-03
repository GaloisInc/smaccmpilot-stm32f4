{-# LANGUAGE FlexibleContexts #-}
--
-- RCC.hs --- RCC (Reset and Clock Control) peripheral driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.RCC
  ( module Ivory.BSP.STM32F405.RCC.Regs
  , module Ivory.BSP.STM32.Peripheral.RCC.RegTypes
  , module Ivory.BSP.STM32.Peripheral.RCC.Regs
  ) where

import Ivory.BSP.STM32F405.RCC.Regs
import Ivory.BSP.STM32.Peripheral.RCC.Regs hiding (RCC_APB1ENR, rcc_apb1enr, rcc_apb1en_pwr) -- Overridden.
import Ivory.BSP.STM32.Peripheral.RCC.RegTypes

