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
  , spiRegDR       :: BitDataReg SPI_DR
  , spiRCCEnable   :: forall eff . Ivory eff ()
  , spiRCCDisable  :: forall eff . Ivory eff ()
  , spiPinMiso     :: GPIOPin
  , spiPinMosi     :: GPIOPin
  , spiPinSck      :: GPIOPin
  , spiPinAF       :: GPIO_AF
  , spiInterrupt   :: Interrupt
  , spiPClk        :: PClk
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
            -> PClk
            -> SPIPeriph
mkSPIPeriph base rccreg rccfield miso mosi sck af inter pclk =
  SPIPeriph
    { spiRegCR1      = mkBitDataReg (base + 0x00)
    , spiRegCR2      = mkBitDataReg (base + 0x04)
    , spiRegSR       = mkBitDataReg (base + 0x08)
    , spiRegDR       = mkBitDataReg (base + 0x0C)
    , spiRCCEnable   = rccEnable  rccreg rccfield
    , spiRCCDisable  = rccDisable rccreg rccfield
    , spiPinMiso     = miso
    , spiPinMosi     = mosi
    , spiPinSck      = sck
    , spiPinAF       = af
    , spiInterrupt   = inter
    , spiPClk        = pclk
    }

spi1, spi2, spi3 :: SPIPeriph
spi1 = mkSPIPeriph spi1_periph_base regRCC_APB2ENR rcc_apb2en_spi1
          pinA7  pinA6  pinA5  gpio_af_spi1 ISR.SPI1 PClk2
spi2 = mkSPIPeriph spi2_periph_base regRCC_APB1ENR rcc_apb1en_spi2
          pinC3  pinC2  pinB10 gpio_af_spi2 ISR.SPI2 PClk1
spi3 = mkSPIPeriph spi3_periph_base regRCC_APB1ENR rcc_apb1en_spi3
          pinC12 pinC11 pinC10 gpio_af_spi3 ISR.SPI3 PClk1

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

-- | Enable peripheral and setup GPIOs. Must be performed
--   before any other SPI peripheral actions.
spiInit :: (eff `AllocsIn` s) => SPIPeriph -> Ivory eff ()
spiInit spi = do
  spiRCCEnable spi
  initInPin  (spiPinMiso spi) (spiPinAF spi)
  initOutPin (spiPinMosi spi) (spiPinAF spi)
  initOutPin (spiPinSck  spi) (spiPinAF spi)

spiInitISR :: (eff `AllocsIn` s) => SPIPeriph -> Uint8 -> Ivory eff ()
spiInitISR spi priority = do
  ISR.interrupt_set_priority inter priority
  ISR.interrupt_enable       inter
  where
  inter = spiInterrupt spi

spiISRHandlerName :: SPIPeriph -> String
spiISRHandlerName spi = ISR.handlerName (spiInterrupt spi)

-- Clock Polarity and Phase: see description
-- of CPOL and CPHA in ST reference manual RM0090
data SPICSActive      = ActiveHigh | ActiveLow
data SPIClockPolarity = ClockPolarityLow | ClockPolarityHigh
data SPIClockPhase    = ClockPhase1 | ClockPhase2
data SPIBitOrder      = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevPeripheral    :: SPIPeriph
  , spiDevCSPin         :: GPIOPin
  , spiDevClockHz       :: Integer
  , spiDevCSActive      :: SPICSActive
  , spiDevClockPolarity :: SPIClockPolarity
  , spiDevClockPhase    :: SPIClockPhase
  , spiDevBitOrder      :: SPIBitOrder
  }

spiDeviceInit :: (eff `AllocsIn` s) => SPIDevice -> Ivory eff ()
spiDeviceInit dev = do
  let pin = spiDevCSPin dev
  pinEnable         pin
  spiDeviceDeselect dev
  pinSetMode        pin gpio_mode_output
  pinSetOutputType  pin gpio_outputtype_pushpull
  pinSetSpeed       pin gpio_speed_2mhz

spiDeviceBegin :: (eff `AllocsIn` s) => SPIDevice -> Ivory eff ()
spiDeviceBegin dev = do
  spiBusEnable
  spiDeviceSelect dev
  -- Enable transfer interrupts:
  modifyReg (spiRegCR2 (spiDevPeripheral dev)) $ do
    let t = fromRep 1
    setField spi_cr2_txeie t
    setField spi_cr2_errie t
  where
  periph = spiDevPeripheral dev
  spiBusEnable = do
    spiModifyCr1        periph [ spi_cr1_spe ] true
    spiClearCr1         periph
    spiClearCr2         periph
    spiModifyCr1        periph [ spi_cr1_mstr, spi_cr1_ssm, spi_cr1_ssi ] true
    baud <- spiDevBaud  periph (spiDevClockHz dev)
    spiSetBaud          periph baud
    spiSetClockPolarity periph (spiDevClockPolarity dev)
    spiSetClockPhase    periph (spiDevClockPhase    dev)
    spiSetBitOrder      periph (spiDevBitOrder      dev)
    spiModifyCr1        periph [ spi_cr1_spe ] true

spiDeviceEnd   :: SPIDevice -> Ivory eff ()
spiDeviceEnd dev = do
  spiDeviceDeselect dev
  spiModifyCr1      periph [ spi_cr1_spe ] false
  where periph = spiDevPeripheral dev


spiSetTXEIE :: SPIPeriph -> Ivory eff ()
spiSetTXEIE spi = modifyReg (spiRegCR2 spi) $ setField spi_cr2_txeie (fromRep 1)

spiClearTXEIE :: SPIPeriph -> Ivory eff ()
spiClearTXEIE spi = modifyReg (spiRegCR2 spi) $ setField spi_cr2_txeie (fromRep 0)

spiSetRXNEIE :: SPIPeriph -> Ivory eff ()
spiSetRXNEIE spi = modifyReg (spiRegCR2 spi) $ setField spi_cr2_rxneie (fromRep 1)

spiClearRXNEIE :: SPIPeriph -> Ivory eff ()
spiClearRXNEIE spi = modifyReg (spiRegCR2 spi) $ setField spi_cr2_rxneie (fromRep 0)

spiGetDR :: SPIPeriph -> Ivory eff Uint8
spiGetDR spi = do
  r <- getReg (spiRegDR spi)
  return (toRep (r #. spi_dr_data))

spiSetDR :: SPIPeriph -> Uint8 -> Ivory eff ()
spiSetDR spi b =
  setReg (spiRegDR spi) $
    setField spi_dr_data (fromRep b)

-- Internal Helper Functions ---------------------------------------------------

spiDevBaud :: (eff `AllocsIn` s)
           => SPIPeriph -> Integer -> Ivory eff SPIBaud
spiDevBaud periph hz = do
  fplk <- getFreqPClk (spiPClk periph)
  let bestWithoutGoingOver = foldl aux spi_baud_div_256 tbl
      target = fromIntegral hz
      aux k (br, brdiv) =
        ((fplk `iDiv` brdiv) <? target) ? (br,k)
  return bestWithoutGoingOver
  where
  tbl = [( spi_baud_div_2,   2)
        ,( spi_baud_div_4,   4)
        ,( spi_baud_div_8,   8)
        ,( spi_baud_div_16,  16)
        ,( spi_baud_div_32,  32)
        ,( spi_baud_div_64,  64)
        ,( spi_baud_div_128, 128)
        ,( spi_baud_div_256, 256)]

spiDeviceSelect   :: SPIDevice -> Ivory eff ()
spiDeviceSelect dev = case spiDevCSActive dev of
  ActiveHigh -> pinSet   (spiDevCSPin dev)
  ActiveLow -> pinClear (spiDevCSPin dev)

spiDeviceDeselect :: SPIDevice -> Ivory eff ()
spiDeviceDeselect dev = case spiDevCSActive dev of
  ActiveHigh -> pinClear (spiDevCSPin dev)
  ActiveLow  -> pinSet   (spiDevCSPin dev)

spiSetBaud :: SPIPeriph -> SPIBaud -> Ivory eff ()
spiSetBaud periph baud = modifyReg (spiRegCR1 periph) $ setField spi_cr1_br baud

spiModifyCr1 :: SPIPeriph -> [BitDataField SPI_CR1 Bit] -> IBool -> Ivory eff ()
spiModifyCr1 periph fields b =
  modifyReg (spiRegCR1 periph) $ mapM_ (\f -> setField f (boolToBit b)) fields

spiClearCr1 :: SPIPeriph -> Ivory eff ()
spiClearCr1 periph = modifyReg (spiRegCR1 periph) $ do
  -- It may not be strictly necessary to clear all of these fields.
  -- I'm copying the implementation of the HWF4 lib, where REG->CR1 is set to 0
  setField spi_cr1_bidimode f
  setField spi_cr1_bidioe   f
  setField spi_cr1_crcen    f
  setField spi_cr1_crcnext  f
  setField spi_cr1_dff      f
  setField spi_cr1_rxonly   f
  setField spi_cr1_ssm      f
  setField spi_cr1_ssi      f
  setField spi_cr1_lsbfirst f
  setField spi_cr1_spe      f
  setField spi_cr1_br       spi_baud_div_2
  setField spi_cr1_mstr     f
  setField spi_cr1_cpol     f
  setField spi_cr1_cpha     f
  where
  f = fromRep 0

spiClearCr2 :: SPIPeriph -> Ivory eff ()
spiClearCr2 periph = modifyReg (spiRegCR2 periph) $ do
  -- May not be strictly necessary to set all these fields, see comment
  -- for spiClearCr1
  setField spi_cr2_txeie    f
  setField spi_cr2_rxneie   f
  setField spi_cr2_errie    f
  setField spi_cr2_ssoe     f
  setField spi_cr2_txdmaen  f
  setField spi_cr2_rxdmaen  f
  where
  f = fromRep 0

spiSetClockPolarity :: SPIPeriph -> SPIClockPolarity -> Ivory eff ()
spiSetClockPolarity periph polarity =
  modifyReg (spiRegCR1 periph) $ setField spi_cr1_cpol b
  where
  b = case polarity of
    ClockPolarityLow  -> fromRep 0
    ClockPolarityHigh -> fromRep 1

spiSetClockPhase :: SPIPeriph -> SPIClockPhase -> Ivory eff ()
spiSetClockPhase periph phase =
  modifyReg (spiRegCR1 periph) $ setField spi_cr1_cpha b
  where
  b = case phase of
    ClockPhase1 -> fromRep 0
    ClockPhase2 -> fromRep 1

spiSetBitOrder :: SPIPeriph -> SPIBitOrder -> Ivory eff ()
spiSetBitOrder periph bitorder =
  modifyReg (spiRegCR1 periph) $ setField spi_cr1_lsbfirst b
  where
  b = case bitorder of
    LSBFirst -> fromRep 1
    MSBFirst -> fromRep 0

