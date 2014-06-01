{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RCC.hs --- the portion of the RCC (Reset and Clock Control) peripehral common
-- to the entire STM32 series (was prev based on F405, its possible some of
-- these are wrong for other chips! but i think i got it right.)
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.RCC
  ( RCC(..)
  , rcc
  , module Ivory.BSP.STM32.Peripheral.RCC.Regs
  , module Ivory.BSP.STM32.Peripheral.RCC.RegTypes
  ) where

import Ivory.HW
import Ivory.BSP.STM32.MemoryMap (rcc_periph_base)
import Ivory.BSP.STM32.Peripheral.RCC.Regs
import Ivory.BSP.STM32.Peripheral.RCC.RegTypes

data RCC =
  RCC
    { rcc_reg_cr        :: BitDataReg RCC_CR
    , rcc_reg_pllcfgr   :: BitDataReg RCC_PLLCFGR
    , rcc_reg_cfgr      :: BitDataReg RCC_CFGR
    , rcc_reg_cir       :: BitDataReg RCC_CIR
    , rcc_reg_apb1enr   :: BitDataReg RCC_APB1ENR
    }

rcc :: RCC
rcc = RCC
  { rcc_reg_cr      = mkBitDataRegNamed (rcc_periph_base + 0x00) "rcc_cr"
  , rcc_reg_pllcfgr = mkBitDataRegNamed (rcc_periph_base + 0x04) "rcc_pllcfgr"
  , rcc_reg_cfgr    = mkBitDataRegNamed (rcc_periph_base + 0x08) "rcc_cfgr"
  , rcc_reg_cir     = mkBitDataRegNamed (rcc_periph_base + 0x0c) "rcc_cir"
  , rcc_reg_apb1enr = mkBitDataRegNamed (rcc_periph_base + 0x40) "rcc_apb1enr"
  }
