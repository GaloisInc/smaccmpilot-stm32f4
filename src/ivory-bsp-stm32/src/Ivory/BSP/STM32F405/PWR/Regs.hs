{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- PWR peripheral registers
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.PWR.Regs where

import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F405.MemoryMap (pwr_periph_base)

import Ivory.BSP.STM32F405.PWR.RegTypes



-- Control Register ------------------------------------------------------------

[bitdata|
 bitdata PWR_CR :: Bits 32 = pwr_cr
  { _                 :: Bits 17
  , pwr_cr_vos        :: Bit
  , _                 :: Bits 4
  , pwr_cr_fpds       :: Bit
  , pwr_cr_dpb        :: Bit
  , pwr_cr_pls        :: PWR_PVD
  , pwr_cr_pvde       :: Bit
  , pwr_cr_csbf       :: Bit
  , pwr_cr_cwuf       :: Bit
  , pwr_cr_pdds       :: Bit
  , pwr_cr_lpds       :: Bit
  }
|]

regPWR_CR :: BitDataReg PWR_CR
regPWR_CR = mkBitDataRegNamed pwr_periph_base "pwr_cr"


-- Control Register ------------------------------------------------------------

[bitdata|
 bitdata PWR_CSR :: Bits 32 = pwr_csr
  { _                 :: Bits 17
  , pwr_csr_vos_rdy   :: Bit
  , _                 :: Bits 4
  , pwr_csr_bre       :: Bit
  , pwr_csr_ewup      :: Bit
  , _                 :: Bits 4
  , pwr_csr_prr       :: Bit
  , pwr_csr_pvdo      :: Bit
  , pwr_csr_sbf       :: Bit
  , pwr_csr_wuf       :: Bit
  }
|]

regPWR_CSR :: BitDataReg PWR_CSR
regPWR_CSR = mkBitDataRegNamed (pwr_periph_base + 0x04) "pwr_csr"


