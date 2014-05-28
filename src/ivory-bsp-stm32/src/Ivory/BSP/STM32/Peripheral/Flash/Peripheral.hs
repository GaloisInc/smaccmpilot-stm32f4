{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
--
-- Peripheral.hs --- Flash peripheral
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.Flash.Peripheral
  ( Flash(..)
  , mkFlash
  ) where

import Ivory.HW

import Ivory.BSP.STM32.Peripheral.Flash.Regs

data Flash = Flash
  { flash_reg_acr     :: BitDataReg FLASH_ACR
  , flash_reg_keyr    :: BitDataReg FLASH_KEYR
  , flash_reg_optkeyr :: BitDataReg FLASH_OPTKEYR
  , flash_reg_sr      :: BitDataReg FLASH_SR
  , flash_reg_cr      :: BitDataReg FLASH_CR
  , flash_reg_optcr   :: BitDataReg FLASH_OPTCR
  }

mkFlash :: Integer -> Flash
mkFlash flash_periph_base =
  Flash
    { flash_reg_acr     = mkBitDataRegNamed (flash_periph_base + 0x00) "flash_acr"
    , flash_reg_keyr    = mkBitDataRegNamed (flash_periph_base + 0x04) "flash_keyr"
    , flash_reg_optkeyr = mkBitDataRegNamed (flash_periph_base + 0x08) "flash_optkeyr"
    , flash_reg_sr      = mkBitDataRegNamed (flash_periph_base + 0x0c) "flash_sr"
    , flash_reg_cr      = mkBitDataRegNamed (flash_periph_base + 0x10) "flash_cr"
    , flash_reg_optcr   = mkBitDataRegNamed (flash_periph_base + 0x14) "flash_optcr"
    }
