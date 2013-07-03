{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
--
-- Peripheral.hs --- I2C peripheral driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.I2C.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.I2C.Regs

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.GPIO.AF
import Ivory.BSP.STM32F4.MemoryMap

import           Ivory.BSP.STM32F4.Interrupt (Interrupt())
import qualified Ivory.BSP.STM32F4.Interrupt as ISR

data I2CPeriph = I2CPeriph
  { i2cRegCR1      :: BitDataReg I2C_CR1
  , i2cRegCR2      :: BitDataReg I2C_CR2
  , i2cRegOAR1     :: BitDataReg I2C_OAR1
  , i2cRegOAR2     :: BitDataReg I2C_OAR2
  , i2cRegDR       :: BitDataReg I2C_DR
  , i2cRegSR1      :: BitDataReg I2C_SR1
  , i2cRegSR2      :: BitDataReg I2C_SR2
  , i2cRegCCR      :: BitDataReg I2C_CCR
  , i2cRegTRISE    :: BitDataReg I2C_TRISE
  , i2cRegFLTR     :: BitDataReg I2C_FLTR
  }

