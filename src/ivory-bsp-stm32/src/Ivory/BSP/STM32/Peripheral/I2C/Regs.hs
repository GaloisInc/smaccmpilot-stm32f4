{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- I2C peripheral registers for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.I2C.Regs where

import Ivory.Language

----------------------------------------------------------------------
-- I2C Registers

[ivory|
 bitdata I2C_CR1 :: Bits 16 = i2c_cr1
   { i2c_cr1_swrst      :: Bit
   , _                  :: Bit
   , i2c_cr1_alert      :: Bit
   , i2c_cr1_pec        :: Bit
   , i2c_cr1_pos        :: Bit
   , i2c_cr1_ack        :: Bit
   , i2c_cr1_stop       :: Bit
   , i2c_cr1_start      :: Bit
   , i2c_cr1_nostretch  :: Bit
   , i2c_cr1_engc       :: Bit
   , i2c_cr1_enpec      :: Bit
   , i2c_cr1_enarp      :: Bit
   , i2c_cr1_smbtype    :: Bit
   , _                  :: Bit
   , i2c_cr1_smbus      :: Bit
   , i2c_cr1_pe         :: Bit
   }

 bitdata I2C_CR2 :: Bits 16 = i2c_cr2
   { _                  :: Bits 3
   , i2c_cr2_last       :: Bit
   , i2c_cr2_dmaen      :: Bit
   , i2c_cr2_itbufen    :: Bit
   , i2c_cr2_itevten    :: Bit
   , i2c_cr2_iterren    :: Bit
   , _                  :: Bits 2
   , i2c_cr2_freq       :: Bits 6
   }

 bitdata I2C_OAR1 :: Bits 16 = i2c_oar1
   { i2c_oar1_addmode   :: Bit
   , _                  :: Bits 5
   , i2c_oar1_add89     :: Bits 2
   , i2c_oar1_add71     :: Bits 7
   , i2c_oar1_add0      :: Bit
   }

 bitdata I2C_OAR2 :: Bits 16 = i2c_oar2
   { _                  :: Bits 8
   , i2c_oar2_add2_71   :: Bits 7
   , i2c_oar2_endual    :: Bit
   }

 bitdata I2C_DR :: Bits 16 = i2c_dr
   { _                  :: Bits 8
   , i2c_dr_data        :: Bits 8
   }

 bitdata I2C_SR1 :: Bits 16 = i2c_sr1
   { i2c_sr1_smbalert   :: Bit
   , i2c_sr1_timeout    :: Bit
   , _                  :: Bit
   , i2c_sr1_pecerr     :: Bit
   , i2c_sr1_ovr        :: Bit
   , i2c_sr1_af         :: Bit
   , i2c_sr1_arlo       :: Bit
   , i2c_sr1_berr       :: Bit
   , i2c_sr1_txe        :: Bit
   , i2c_sr1_rxne       :: Bit
   , _                  :: Bit
   , i2c_sr1_stopf      :: Bit
   , i2c_sr1_add10      :: Bit
   , i2c_sr1_btf        :: Bit
   , i2c_sr1_addr       :: Bit
   , i2c_sr1_sb         :: Bit
   }

 bitdata I2C_SR2 :: Bits 16 = i2c_sr2
   { i2c_sr2_pec        :: Bits 8
   , i2c_sr2_dualf       :: Bit
   , i2c_sr2_smbhost     :: Bit
   , i2c_sr2_smbdflt     :: Bit
   , i2c_sr2_gencall     :: Bit
   , _                   :: Bit
   , i2c_sr2_tra         :: Bit
   , i2c_sr2_busy        :: Bit
   , i2c_sr2_msl         :: Bit
   }

 bitdata I2C_CCR :: Bits 16 = i2c_ccr
   { i2c_ccr_fastmode   :: Bit
   , i2c_ccr_duty       :: Bit
   , _                  :: Bits 2
   , i2c_ccr_ccr        :: Bits 12
   }

 bitdata I2C_TRISE :: Bits 16 = i2c_trise
   { _                    :: Bits 10
   , i2c_trise_trise      :: Bits 6
   }

 bitdata I2C_FLTR :: Bits 16 = i2c_fltr
   { _                    :: Bits 11
   , i2c_fltr_anoff       :: Bit
   , i2c_fltr_dnf         :: Bits 4
   }
|]

