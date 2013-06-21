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
import Ivory.BSP.STM32F4.GPIO

data SPIBitOrder = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevCSPin         :: GPIOPin
  , spiDevCSActive      :: Bool        -- CS active high?
  , spiDevBaud          :: SPIBaud
  , spiDevClockPolarity :: Bool
  , spiDevClockPhase    :: Bool
  , spiDevBitOrder      :: SPIBitOrder
  }

