{-# LANGUAGE DataKinds #-}

--
-- Peripheral.hs --- PWR peripheral
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.PWR.Peripheral where

import Ivory.HW
import Ivory.BSP.STM32.Peripheral.PWR.Regs

data PWR = PWR
  { pwr_reg_cr  :: BitDataReg PWR_CR
  , pwr_reg_csr :: BitDataReg PWR_CSR
  }

mkPwr :: Integer -> PWR
mkPwr periph_base = PWR
  { pwr_reg_cr  = mkBitDataRegNamed  periph_base         "pwr_cr"
  , pwr_reg_csr = mkBitDataRegNamed (periph_base + 0x04) "pwr_csr"
  }
