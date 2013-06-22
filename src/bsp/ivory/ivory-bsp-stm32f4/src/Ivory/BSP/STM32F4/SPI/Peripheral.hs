{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
--
-- Peripheral.hs --- SPI peripheral driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.SPI.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.SPI.RegTypes
import Ivory.BSP.STM32F4.SPI.Regs

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.GPIO.AF
import Ivory.BSP.STM32F4.MemoryMap

import           Ivory.BSP.STM32F4.Interrupt (Interrupt())
import qualified Ivory.BSP.STM32F4.Interrupt as ISR

data SPIPeriph = SPIPeriph
  { spiRegCR1      :: BitDataReg SPI_CR1
  , spiRegCR2      :: BitDataReg SPI_CR2
  , spiRegSR       :: BitDataReg SPI_SR
  , spiRegDR       :: Reg Uint16
  , spiRCCEnable   :: forall eff . Ivory eff ()
  , spiRCCDisable  :: forall eff . Ivory eff ()
  , spiPinMiso     :: GPIOPin
  , spiPinMosi     :: GPIOPin
  , spiPinSck      :: GPIOPin
  , spiPinAF       :: GPIO_AF
  , spiInterrupt   :: Interrupt
  }

mkSPIPeriph :: (BitData a, IvoryIOReg (BitDataRep a))
            => Integer
            -> BitDataReg a
            -> BitDataField a Bit
            -> GPIOPin
            -> GPIOPin
            -> GPIOPin
            -> GPIO_AF
            -> Interrupt
            -> SPIPeriph
mkSPIPeriph base rccreg rccfield miso mosi sck af inter =
  SPIPeriph
    { spiRegCR1      = mkBitDataReg (base + 0x00)
    , spiRegCR2      = mkBitDataReg (base + 0x04)
    , spiRegSR       = mkBitDataReg (base + 0x08)
    , spiRegDR       = mkReg (base + 0x0C)
    , spiRCCEnable   = rccEnable  rccreg rccfield
    , spiRCCDisable  = rccDisable rccreg rccfield
    , spiPinMiso     = miso
    , spiPinMosi     = mosi
    , spiPinSck      = sck
    , spiPinAF       = af
    , spiInterrupt   = inter
    }

spi1, spi2, spi3 :: SPIPeriph
spi1 = mkSPIPeriph spi1_periph_base regRCC_APB2ENR rcc_apb2en_spi1
          pinA7  pinA6  pinA5  gpio_af_spi1 ISR.SPI1
spi2 = mkSPIPeriph spi2_periph_base regRCC_APB1ENR rcc_apb1en_spi2
          pinC3  pinC2  pinB10 gpio_af_spi2 ISR.SPI2
spi3 = mkSPIPeriph spi3_periph_base regRCC_APB1ENR rcc_apb1en_spi3
          pinC12 pinC11 pinC10 gpio_af_spi3 ISR.SPI3

initInPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initInPin pin af = do
  pinEnable  pin
  pinSetAF   pin af
  pinSetMode pin gpio_mode_af
  pinSetPUPD pin gpio_pupd_none

initOutPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initOutPin pin af = do
  pinEnable        pin
  pinSetAF         pin af
  pinSetMode       pin gpio_mode_af
  pinSetOutputType pin gpio_outputtype_pushpull

-- | Enable peripheral and setup GPIOs
spiInit :: (eff `AllocsIn` s) => SPIPeriph -> Ivory eff ()
spiInit spi = do
  spiRCCEnable spi
  initInPin  (spiPinMiso spi) (spiPinAF spi)
  initOutPin (spiPinMosi spi) (spiPinAF spi)
  initOutPin (spiPinSck  spi) (spiPinAF spi)


data SPIBitOrder = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevPeripheral    :: SPIPeriph
  , spiDevCSPin         :: GPIOPin
  , spiDevClockHz       :: Integer
  , spiDevCSActive      :: Bool -- CS active high?
  , spiDevClockPolarity :: Bool
  , spiDevClockPhase    :: Bool
  , spiDevBitOrder      :: SPIBitOrder
  }

spiDevBaud :: Integer -> Ivory eff SPIBaud
spiDevBaud hz = return spi_baud_div_128 -- XXX fix with real calculation based on apb clocks

spiDeviceSelect   :: SPIDevice -> Ivory eff ()
spiDeviceSelect dev = case spiDevCSActive dev of
  True  -> pinSet   (spiDevCSPin dev)
  False -> pinClear (spiDevCSPin dev)

spiDeviceDeselect :: SPIDevice -> Ivory eff ()
spiDeviceDeselect dev = case spiDevCSActive dev of
  True  -> pinClear (spiDevCSPin dev)
  False -> pinSet   (spiDevCSPin dev)

spiDeviceInit :: (eff `AllocsIn` s) => SPIDevice -> Ivory eff ()
spiDeviceInit dev = do
  let pin = spiDevCSPin dev
  pinEnable         pin
  spiDeviceDeselect dev
  pinSetMode        pin gpio_mode_output
  pinSetOutputType  pin gpio_outputtype_pushpull
  pinSetSpeed       pin gpio_speed_2mhz

spiDeviceBegin :: SPIDevice -> Ivory eff ()
spiDeviceBegin dev = return () -- XXX fixme

spiDeviceEnd   :: SPIDevice -> Ivory eff ()
spiDeviceEnd dev = return () -- XXX fixme


