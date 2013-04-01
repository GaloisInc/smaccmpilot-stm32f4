{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
--
-- SPI.hs --- SPI driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.SPI where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.SPITypes
import Ivory.BSP.STM32F4.GPIO

----------------------------------------------------------------------
-- SPI Registers

[bitdata|
 bitdata SPI_CR1 :: Bits 16 = spi_cr1
   { spi_cr1_cpha     :: Bit
   , spi_cr1_cpol     :: Bit
   , spi_cr1_mstr     :: Bit
   , spi_cr1_br       :: SPIBaud
   , spi_cr1_spe      :: Bit
   , spi_cr1_lsbfirst :: Bit
   , spi_cr1_ssi      :: Bit
   , spi_cr1_ssm      :: Bit
   , spi_cr1_rxonly   :: Bit
   , spi_cr1_dff      :: Bit
   , spi_cr1_crcnext  :: Bit
   , spi_cr1_crcen    :: Bit
   , spi_cr1_bidioe   :: Bit
   , spi_cr1_bidimode :: Bit
   }

 bitdata SPI_CR2 :: Bits 16 = spi_cr2
   { spi_cr2_rxdmaen  :: Bit
   , spi_cr2_txdmaen  :: Bit
   , spi_cr2_ssoe     :: Bit
   , _                :: Bits 2
   , spi_cr2_errie    :: Bit
   , spi_cr2_rxneie   :: Bit
   , spi_cr2_txeie    :: Bit
   , _                :: Bits 8
   }

  bitdata SPI_SR :: Bits 16 = spi_sr
   { spi_sr_rxne      :: Bit
   , spi_sr_txe       :: Bit
   , spi_sr_chside    :: Bit
   , spi_sr_udr       :: Bit
   , spi_sr_crcerr    :: Bit
   , spi_sr_modf      :: Bit
   , spi_sr_ovr       :: Bit
   , spi_sr_bsy       :: Bit
   }
|]

data SPIBus = SPIBus
  { spiRegCR1 :: BitDataReg SPI_CR1
  , spiRegCR2 :: BitDataReg SPI_CR2
  , spiRegSR  :: BitDataReg SPI_SR
  , spiRegDR  :: Reg Uint16
  }

data SPIBitOrder = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevCSPin         :: GPIOPin
  , spiDevCSActive      :: Bool        -- CS active high?
  , spiDevBaud          :: SPIBaud
  , spiDevClockPolarity :: Bool
  , spiDevClockPhase    :: Bool
  , spiDevBitOrder      :: SPIBitOrder
  }

-- | Construct a "SPIBus" given a base address for the registers.
mkSPIBus :: Integer -> SPIBus
mkSPIBus base =
  SPIBus
    { spiRegCR1 = mkBitDataReg (base + 0x00)
    , spiRegCR2 = mkBitDataReg (base + 0x04)
    , spiRegSR  = mkBitDataReg (base + 0x08)
    , spiRegDR  = mkReg (base + 0x0C)
    }

spi1Bus, spi2Bus, spi3Bus :: SPIBus
spi1Bus = mkSPIBus 0x40013000
spi2Bus = mkSPIBus 0x40003800
spi3Bus = mkSPIBus 0x40003C00
