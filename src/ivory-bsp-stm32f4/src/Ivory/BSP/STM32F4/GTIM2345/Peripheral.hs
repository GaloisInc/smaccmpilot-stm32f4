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
       => Integer -> BitDataField RCC_APB1ENR Bit -> String -> GTIM a
mkGTIM base rccfield n =
  GTIM
    { gtimRegCR1         = reg 0x00 "cr1"
    , gtimRegCR2         = reg 0x04 "cr2"
    , gtimRegSMCR        = reg 0x08 "smcr"
    , gtimRegDIER        = reg 0x0C "dier"
    , gtimRegSR          = reg 0x10 "sr"
    , gtimRegEGR         = reg 0x14 "egr"
    , gtimRegCCMR1_OCM   = reg 0x18 "ccmr1_ocm" -- aliased with icm
    , gtimRegCCMR1_ICM   = reg 0x18 "ccmr1_icm"
    , gtimRegCCMR2_OCM   = reg 0x1C "ccmr2_ocm" -- aliased with icm
    , gtimRegCCMR2_ICM   = reg 0x1C "ccmr2_icm"
    , gtimRegCCER        = reg 0x20 "ccer"
    , gtimRegCNT         = reg 0x24 "cnt"
    , gtimRegPSC         = reg 0x28 "psc"
    , gtimRegARR         = reg 0x2C "arr"
    , gtimRegCCR1        = reg 0x34 "ccr1"
    , gtimRegCCR2        = reg 0x38 "ccr2"
    , gtimRegCCR3        = reg 0x3C "ccr3"
    , gtimRegCCR4        = reg 0x40 "ccr4"
    , gtimRCCEnable      = rccEnable  rccreg rccfield
    , gtimRCCDisable     = rccDisable rccreg rccfield
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)
  rccreg = regRCC_APB1ENR -- All TIM2345 are in APB1

tim2 :: GTIM16
tim2 = mkGTIM tim2_periph_base rcc_apb1en_tim2 "tim2"

tim3 :: GTIM16
tim3 = mkGTIM tim3_periph_base rcc_apb1en_tim3 "tim3"

tim4 :: GTIM16
tim4 = mkGTIM tim4_periph_base rcc_apb1en_tim4 "tim4"

tim5 :: GTIM16
tim5 = mkGTIM tim5_periph_base rcc_apb1en_tim5 "tim5"

-- Both TIM2 and TIM5 are really 32 bit timers, but you can safely make
-- believe they are 16 bit.

tim2_32 :: GTIM32
tim2_32 = mkGTIM tim2_periph_base rcc_apb1en_tim2 "tim2"

tim5_32 :: GTIM32
tim5_32 = mkGTIM tim5_periph_base rcc_apb1en_tim5 "tim5"

