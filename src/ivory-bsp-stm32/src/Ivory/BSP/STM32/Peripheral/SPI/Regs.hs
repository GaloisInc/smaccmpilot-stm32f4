{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- SPI peripheral registers for the STM32
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.SPI.Regs where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.SPI.RegTypes

----------------------------------------------------------------------
-- SPI Registers

[ivory|
 bitdata SPI_CR1 :: Bits 16 = spi_cr1
   { spi_cr1_bidimode :: Bit
   , spi_cr1_bidioe   :: Bit
   , spi_cr1_crcen    :: Bit
   , spi_cr1_crcnext  :: Bit
   , spi_cr1_dff      :: Bit
   , spi_cr1_rxonly   :: Bit
   , spi_cr1_ssm      :: Bit
   , spi_cr1_ssi      :: Bit
   , spi_cr1_lsbfirst :: Bit
   , spi_cr1_spe      :: Bit
   , spi_cr1_br       :: SPIBaud
   , spi_cr1_mstr     :: Bit
   , spi_cr1_cpol     :: Bit
   , spi_cr1_cpha     :: Bit
   }

 bitdata SPI_CR2 :: Bits 16 = spi_cr2
   { _                :: Bits 8
   , spi_cr2_txeie    :: Bit
   , spi_cr2_rxneie   :: Bit
   , spi_cr2_errie    :: Bit
   , _                :: Bits 2
   , spi_cr2_ssoe     :: Bit
   , spi_cr2_txdmaen  :: Bit
   , spi_cr2_rxdmaen  :: Bit
   }

  bitdata SPI_SR :: Bits 16 = spi_sr
   { _                :: Bits 7
   , spi_sr_fre       :: Bit
   , spi_sr_bsy       :: Bit
   , spi_sr_ovr       :: Bit
   , spi_sr_modf      :: Bit
   , spi_sr_crcerr    :: Bit
   , spi_sr_udr       :: Bit
   , spi_sr_chside    :: Bit
   , spi_sr_txe       :: Bit
   , spi_sr_rxne      :: Bit
   }

  bitdata SPI_DR :: Bits 16 = spi_dr
   { _                :: Bits 8
   , spi_dr_data      :: Bits 8
   }
|]

