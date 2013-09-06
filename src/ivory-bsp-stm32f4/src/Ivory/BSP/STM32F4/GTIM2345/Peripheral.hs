{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
--
-- Peripheral.hs --- General Purpose Timer (TIM2 to TIM5) Peripheral driver.
-- Defines peripheral types, instances, and public API.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GTIM2345.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.GTIM2345.Regs
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.MemoryMap

-- Convenience type synonyms
type GTIM32 = GTIM GTIM_32
type GTIM16 = GTIM GTIM_16

data GTIM a = GTIM
  { gtimRegCR1          :: BitDataReg GTIM_CR1
  , gtimRegCR2          :: BitDataReg GTIM_CR2
  , gtimRegSMCR         :: BitDataReg GTIM_SMCR
  , gtimRegDIER         :: BitDataReg GTIM_DIER
  , gtimRegSR           :: BitDataReg GTIM_SR
  , gtimRegEGR          :: BitDataReg GTIM_EGR
  , gtimRegCCMR1_OCM    :: BitDataReg GTIM_CCMR1_OCM
  , gtimRegCCMR1_ICM    :: BitDataReg GTIM_CCMR1_ICM
  , gtimRegCCMR2_OCM    :: BitDataReg GTIM_CCMR2_OCM
  , gtimRegCCMR2_ICM    :: BitDataReg GTIM_CCMR2_ICM
  , gtimRegCCER         :: BitDataReg GTIM_CCER
  , gtimRegCNT          :: BitDataReg a
  , gtimRegPSC          :: BitDataReg GTIM_16
  , gtimRegARR          :: BitDataReg a
  , gtimRegCCR1         :: BitDataReg a
  , gtimRegCCR2         :: BitDataReg a
  , gtimRegCCR3         :: BitDataReg a
  , gtimRegCCR4         :: BitDataReg a
  , gtimRCCEnable       :: forall eff . Ivory eff ()
  , gtimRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create a GPIO port given the base register address.
mkGTIM :: (IvoryIOReg (BitDataRep a))
       => Integer -> BitDataField RCC_APB1ENR Bit -> GTIM a
mkGTIM base rccfield =
  GTIM
    { gtimRegCR1         = mkBitDataReg $ base + 0x00
    , gtimRegCR2         = mkBitDataReg $ base + 0x04
    , gtimRegSMCR        = mkBitDataReg $ base + 0x08
    , gtimRegDIER        = mkBitDataReg $ base + 0x0C
    , gtimRegSR          = mkBitDataReg $ base + 0x10
    , gtimRegEGR         = mkBitDataReg $ base + 0x14
    , gtimRegCCMR1_OCM   = mkBitDataReg $ base + 0x18 -- aliased with icm
    , gtimRegCCMR1_ICM   = mkBitDataReg $ base + 0x18
    , gtimRegCCMR2_OCM   = mkBitDataReg $ base + 0x1C -- aliased with icm
    , gtimRegCCMR2_ICM   = mkBitDataReg $ base + 0x1C
    , gtimRegCCER        = mkBitDataReg $ base + 0x20
    , gtimRegCNT         = mkBitDataReg $ base + 0x24
    , gtimRegPSC         = mkBitDataReg $ base + 0x28
    , gtimRegARR         = mkBitDataReg $ base + 0x2C
    , gtimRegCCR1        = mkBitDataReg $ base + 0x34
    , gtimRegCCR2        = mkBitDataReg $ base + 0x38
    , gtimRegCCR3        = mkBitDataReg $ base + 0x3C
    , gtimRegCCR4        = mkBitDataReg $ base + 0x40
    , gtimRCCEnable      = rccEnable  rccreg rccfield
    , gtimRCCDisable     = rccDisable rccreg rccfield
    }
  where rccreg = regRCC_APB1ENR -- All TIM2345 are in APB1

tim2 :: GTIM16
tim2 = mkGTIM tim2_periph_base rcc_apb1en_tim2

tim3 :: GTIM16
tim3 = mkGTIM tim3_periph_base rcc_apb1en_tim3

tim4 :: GTIM16
tim4 = mkGTIM tim4_periph_base rcc_apb1en_tim4

tim5 :: GTIM16
tim5 = mkGTIM tim5_periph_base rcc_apb1en_tim5

-- Both TIM2 and TIM5 are really 32 bit timers, but you can safely make
-- believe they are 16 bit.

tim2_32 :: GTIM32
tim2_32 = mkGTIM tim2_periph_base rcc_apb1en_tim2

tim5_32 :: GTIM32
tim5_32 = mkGTIM tim5_periph_base rcc_apb1en_tim5

