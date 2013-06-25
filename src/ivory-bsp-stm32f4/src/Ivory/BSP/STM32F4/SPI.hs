
module Ivory.BSP.STM32F4.SPI
  -- Peripheral definitions:
  ( SPIPeriph()
  , spi1, spi2, spi3
  -- Device definitions:
  , SPIDevice(..)
  , SPICSActive(..)
  , SPIClockPolarity(..)
  , SPIClockPhase(..)
  , SPIBitOrder(..)
  -- Peripheral API:
  , spiInit
  , spiInitISR
  , spiISRHandlerName
  -- Device API:
  , spiDeviceInit
  , spiDeviceBegin
  , spiDeviceEnd
  ) where

import Ivory.BSP.STM32F4.SPI.Peripheral
