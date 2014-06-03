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

module Ivory.BSP.STM32.Peripheral.GTIM2345.Peripheral where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GTIM2345.Regs

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
       => Integer -- Base Addr
       -> (forall eff . Ivory eff ()) -- RCC Enable
       -> (forall eff . Ivory eff ()) -- RCC Disable
       -> String
       -> GTIM a
mkGTIM base rccen rccdis n =
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
    , gtimRCCEnable      = rccen
    , gtimRCCDisable     = rccdis
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

