{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
--
-- Peripheral.hs --- Advanced Timer (TIM1 to TIM8) Peripheral driver.
-- Defines peripheral types, instances.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.ATIM18.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.ATIM18.Regs
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.MemoryMap

-- Convenience type synonyms
data ATIM = ATIM
  { atimRegCR1          :: BitDataReg ATIM_CR1
  , atimRegCR2          :: BitDataReg ATIM_CR2
  , atimRegSMCR         :: BitDataReg ATIM_SMCR
  , atimRegDIER         :: BitDataReg ATIM_DIER
  , atimRegSR           :: BitDataReg ATIM_SR
  , atimRegEGR          :: BitDataReg ATIM_EGR
  , atimRegCCMR1_OCM    :: BitDataReg ATIM_CCMR1_OCM
  , atimRegCCMR1_ICM    :: BitDataReg ATIM_CCMR1_ICM
  , atimRegCCMR2_OCM    :: BitDataReg ATIM_CCMR2_OCM
  , atimRegCCMR2_ICM    :: BitDataReg ATIM_CCMR2_ICM
  , atimRegCCER         :: BitDataReg ATIM_CCER
  , atimRegCNT          :: BitDataReg ATIM_16
  , atimRegPSC          :: BitDataReg ATIM_PSC
  , atimRegARR          :: BitDataReg ATIM_16
  , atimRegCCR1         :: BitDataReg ATIM_16
  , atimRegCCR2         :: BitDataReg ATIM_16
  , atimRegCCR3         :: BitDataReg ATIM_16
  , atimRegCCR4         :: BitDataReg ATIM_16
  , atimRegBDTR         :: BitDataReg ATIM_BDTR
  , atimRCCEnable       :: forall eff . Ivory eff ()
  , atimRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create an ATIM given the base register address.
mkATIM :: Integer -> BitDataField RCC_APB2ENR Bit -> String -> ATIM
mkATIM base rccfield n =
  ATIM
    { atimRegCR1         = reg 0x00 "cr1"
    , atimRegCR2         = reg 0x04 "cr2"
    , atimRegSMCR        = reg 0x08 "smcr"
    , atimRegDIER        = reg 0x0C "dier"
    , atimRegSR          = reg 0x10 "sr"
    , atimRegEGR         = reg 0x14 "egr"
    , atimRegCCMR1_OCM   = reg 0x18 "ccmr1_ocm" -- aliased with icm
    , atimRegCCMR1_ICM   = reg 0x18 "ccmr1_icm"
    , atimRegCCMR2_OCM   = reg 0x1C "ccmr2_ocm" -- aliased with icm
    , atimRegCCMR2_ICM   = reg 0x1C "ccmr2_icm"
    , atimRegCCER        = reg 0x20 "ccer"
    , atimRegCNT         = reg 0x24 "cnt"
    , atimRegPSC         = reg 0x28 "psc"
    , atimRegARR         = reg 0x2C "arr"
    , atimRegCCR1        = reg 0x34 "ccr1"
    , atimRegCCR2        = reg 0x38 "ccr2"
    , atimRegCCR3        = reg 0x3C "ccr3"
    , atimRegCCR4        = reg 0x40 "ccr4"
    , atimRegBDTR        = reg 0x44 "bdtr"
    , atimRCCEnable      = rccEnable  rccreg rccfield
    , atimRCCDisable     = rccDisable rccreg rccfield
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)
  rccreg = regRCC_APB2ENR -- TIM1 and TIM8 are in APB2

tim1 :: ATIM
tim1 = mkATIM tim1_periph_base rcc_apb2en_tim1 "tim1"

tim8 :: ATIM
tim8 = mkATIM tim8_periph_base rcc_apb2en_tim8 "tim8"

