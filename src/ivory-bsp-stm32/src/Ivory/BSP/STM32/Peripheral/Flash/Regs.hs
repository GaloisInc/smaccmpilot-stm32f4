{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- Flash peripheral registers
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.Flash.Regs where

import Ivory.Language

-- Access Control Register ----------------------------------------------------

[ivory|
 bitdata FLASH_ACR :: Bits 32 = flash_acr
  { _                 :: Bits 19
  , flash_acr_dc_rst  :: Bit
  , flash_acr_ic_rst  :: Bit
  , flash_acr_dc_en   :: Bit
  , flash_acr_ic_en   :: Bit
  , flash_acr_prft_en :: Bit
  , _                 :: Bits 5
  , flash_acr_latency :: Bits 3
  }
|]

-- Key Register ----------------------------------------------------------------

[ivory|
 bitdata FLASH_KEYR :: Bits 32 = flash_keyr
  { flash_keyr_key     :: Bits 32
  }
|]

-- Option Key Register ---------------------------------------------------------

[ivory|
 bitdata FLASH_OPTKEYR :: Bits 32 = flash_optkeyr
  { flash_optkeyr_key     :: Bits 32
  }
|]

-- Status Register -------------------------------------------------------------

[ivory|
 bitdata FLASH_SR :: Bits 32 = flash_sr
  { _                :: Bits 15
  , flash_sr_bsy     :: Bit
  , _                :: Bits 8
  , flash_sr_psg_err :: Bit
  , flash_sr_pgp_err :: Bit
  , flash_sr_pga_err :: Bit
  , flash_sr_wpr_err :: Bit
  , _                :: Bits 2
  , flash_sr_op_err  :: Bit
  , flash_sr_eop     :: Bit
  }
|]

-- Control Register ------------------------------------------------------------

[ivory|
 bitdata FLASH_CR :: Bits 32 = flash_cr
  { flash_cr_lock    :: Bit
  , _                :: Bits 5
  , flash_cr_err_ie  :: Bit
  , flash_cr_eop_ie  :: Bit
  , _                :: Bits 7
  , flash_cr_strt    :: Bit
  , _                :: Bits 6
  , flash_cr_psize   :: Bits 2
  , _                :: Bit
  , flash_cr_snb     :: Bits 4
  , flash_cr_mer     :: Bit
  , flash_cr_ser     :: Bit
  , flash_cr_pg      :: Bit
  }
|]

-- Option Control Register -----------------------------------------------------

[ivory|
 bitdata FLASH_OPTCR :: Bits 32 = flash_optcr
  { _                       :: Bits 4
  , flash_optcr_nwrp        :: Bits 12
  , flash_optcr_rdp         :: Bits 8
  , flash_optcr_nrst_stdby  :: Bit
  , flash_optcr_nrst_stop   :: Bit
  , flash_optcr_wdg_sw      :: Bit
  , _                       :: Bit
  , flash_optcr_bor_lev     :: Bits 2
  , flash_optcr_opt_strt    :: Bit
  , flash_optcr_opt_lock    :: Bit
  }
|]

