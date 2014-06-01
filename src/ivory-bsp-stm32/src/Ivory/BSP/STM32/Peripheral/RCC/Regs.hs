{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- RCC (Reset and Clock Control) registers, for entire STM32 series
-- (was prev based on F405, its possible some of these are wrong for other
-- chips! but i think i got it right.)
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.RCC.Regs where

import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32.MemoryMap (rcc_periph_base)

import Ivory.BSP.STM32.Peripheral.RCC.RegTypes


-- Control Register ------------------------------------------------------------

[bitdata|
 bitdata RCC_CR :: Bits 32 = rcc_cr
  { _                 :: Bits 4
  , rcc_cr_plli2s_rdy :: Bit
  , rcc_cr_plli2s_on  :: Bit
  , rcc_cr_pll_rdy    :: Bit
  , rcc_cr_pll_on     :: Bit
  , _                 :: Bits 4
  , rcc_cr_css_on     :: Bit
  , rcc_cr_hse_byp    :: Bit
  , rcc_cr_hse_rdy    :: Bit
  , rcc_cr_hse_on     :: Bit
  , rcc_cr_hsical     :: Bits 8
  , rcc_cr_hsitrim    :: Bits 5
  , _                 :: Bit
  , rcc_cr_hsi_rdy    :: Bit
  , rcc_cr_hsi_on     :: Bit
  }
|]

regRCC_CR :: BitDataReg RCC_CR
regRCC_CR = mkBitDataRegNamed rcc_periph_base "rcc_cr"

-- PLL Configuration Register --------------------------------------------------

[bitdata|
 bitdata RCC_PLLCFGR :: Bits 32 = rcc_pllcfgr
  { rcc_pllcfgr_pllq      :: Bits 4 --  2 <= pllq <= 15
  , _                     :: Bit
  , rcc_pllcfgr_pllsrc    :: Bit
  , _                     :: Bits 4
  , rcc_pllcfgr_pllp      :: RCC_PLLP
  , _                     :: Bit
  , rcc_pllcfgr_plln      :: Bits 9 -- 192 <= plln <= 432
  , rcc_pllcfgr_pllm      :: Bits 6 --   2 <= pllm <= 63
  }
|]

regRCC_PLLCFGR :: BitDataReg RCC_PLLCFGR
regRCC_PLLCFGR = mkBitDataRegNamed (rcc_periph_base + 0x04) "rcc_pllcfgr"

-- Clock Configuration Register ------------------------------------------------

[bitdata|
 bitdata RCC_CFGR :: Bits 32 = rcc_cfgr
  { rcc_cfgr_mco2         :: RCC_MCOx
  , rcc_cfgr_mco2_pre     :: RCC_MCOxPre
  , rcc_cfgr_mco1_pre     :: RCC_MCOxPre
  , rcc_cfgr_i2ssrc       :: Bit
  , rcc_cfgr_mco1         :: RCC_MCOx
  , rcc_cfgr_rtcpre       :: Bits 5
  , rcc_cfgr_ppre2        :: RCC_PPREx
  , rcc_cfgr_ppre1        :: RCC_PPREx
  , _                     :: Bits 2
  , rcc_cfgr_hpre         :: RCC_HPRE
  , rcc_cfgr_sws          :: RCC_SYSCLK
  , rcc_cfgr_sw           :: RCC_SYSCLK
  }
|]

regRCC_CFGR :: BitDataReg RCC_CFGR
regRCC_CFGR = mkBitDataRegNamed (rcc_periph_base + 0x08) "rcc_cfgr"

-- Clock Configuration Register ------------------------------------------------

[bitdata|
 bitdata RCC_CIR :: Bits 32 = rcc_cir
  { _                    :: Bits 8
  , rcc_cir_cssc         :: Bit
  , _                    :: Bit
  , rcc_cir_plli2s_rdyc  :: Bit
  , rcc_cir_pll_rdyc     :: Bit
  , rcc_cir_hse_rdyc     :: Bit
  , rcc_cir_hsi_rdyc     :: Bit
  , rcc_cir_lse_rdyc     :: Bit
  , rcc_cir_lsi_rdyc     :: Bit
  , _                    :: Bits 2
  , rcc_cir_plli2s_rdyie :: Bit
  , rcc_cir_pll_rdyie    :: Bit
  , rcc_cir_hse_rdyie    :: Bit
  , rcc_cir_hsi_rdyie    :: Bit
  , rcc_cir_lse_rdyie    :: Bit
  , rcc_cir_lsi_rdyie    :: Bit
  , rcc_cir_cssf         :: Bit
  , _                    :: Bit
  , rcc_cir_plli2s_rdyf  :: Bit
  , rcc_cir_pll_rdyf     :: Bit
  , rcc_cir_hse_rdyf     :: Bit
  , rcc_cir_hsi_rdyf     :: Bit
  , rcc_cir_lse_rdyf     :: Bit
  , rcc_cir_lsi_rdyf     :: Bit
  }
|]

regRCC_CIR :: BitDataReg RCC_CIR
regRCC_CIR = mkBitDataRegNamed (rcc_periph_base + 0x0c) "rcc_cir"

