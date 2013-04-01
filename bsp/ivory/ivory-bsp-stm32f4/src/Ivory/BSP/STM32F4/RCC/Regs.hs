{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
--
-- Regs.hs --- RCC registers.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC.Regs where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

[bitdata|
 bitdata RCC_AHB1ENR :: Bits 32 = rcc_ahb1enr
  -- not defining the other bits until needed
  { _                :: Bits 23
  , rcc_ahb1en_gpioi :: Bit
  , rcc_ahb1en_gpioh :: Bit
  , rcc_ahb1en_gpiog :: Bit
  , rcc_ahb1en_gpiof :: Bit
  , rcc_ahb1en_gpioe :: Bit
  , rcc_ahb1en_gpiod :: Bit
  , rcc_ahb1en_gpioc :: Bit
  , rcc_ahb1en_gpiob :: Bit
  , rcc_ahb1en_gpioa :: Bit
  }
|]

regRCC_AHB1ENR :: BitDataReg RCC_AHB1ENR
regRCC_AHB1ENR = mkBitDataReg 0x40023830
