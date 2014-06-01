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
  , rccEnable
  , rccDisable
  -- * system clock frequency
  , PClk(..)

  , getFreqSysClk
  , getFreqHClk
  , getFreqPClk1
  , getFreqPClk2
  , getFreqPClk
  ) where

import Ivory.BSP.STM32F405.RCC.Regs
import Ivory.BSP.STM32.Peripheral.RCC.Regs hiding (RCC_APB1ENR, rcc_apb1enr, rcc_apb1en_pwr) -- Overridden.
import Ivory.BSP.STM32.Peripheral.RCC.RegTypes
import Ivory.BSP.STM32F405.RCC.GetFreq

import Ivory.Language
import Ivory.BitData
import Ivory.HW

rccEnable :: (BitData a, IvoryIOReg (BitDataRep a))
          => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccEnable reg field = modifyReg reg (setBit field)

rccDisable :: (BitData a, IvoryIOReg (BitDataRep a))
           => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccDisable reg field = modifyReg reg (clearBit field)

