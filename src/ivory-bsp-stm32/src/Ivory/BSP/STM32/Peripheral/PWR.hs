{-# LANGUAGE DataKinds #-}

--
-- PWR.hs --- PWR peripheral
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.PWR
  ( PWR(..)
  , pwr
  , module Ivory.BSP.STM32.Peripheral.PWR.Regs
  ) where

import Ivory.HW
import Ivory.BSP.STM32.Peripheral.PWR.Regs
import Ivory.BSP.STM32.MemoryMap (pwr_periph_base)

data PWR = PWR
  { pwr_reg_cr  :: BitDataReg PWR_CR
  , pwr_reg_csr :: BitDataReg PWR_CSR
  }

pwr :: PWR
pwr = PWR
  { pwr_reg_cr  = mkBitDataRegNamed  pwr_periph_base         "pwr_cr"
  , pwr_reg_csr = mkBitDataRegNamed (pwr_periph_base + 0x04) "pwr_csr"
  }
