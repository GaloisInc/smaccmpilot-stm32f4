{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PX4.Tests.Platforms where

import Tower.Config

import Data.Char (toUpper)

import qualified BSP.Tests.Platforms as BSP

import qualified Ivory.BSP.STM32F405.UART           as F405
import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.SPI            as F405
import qualified Ivory.BSP.STM32F405.I2C            as F405
import qualified Ivory.BSP.STM32F405.Interrupt      as F405
import           Ivory.BSP.STM32.Peripheral.GPIOF4
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.SPI
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Driver.I2C
import           Ivory.OS.FreeRTOS.Tower.STM32.Config

data PX4Platform s =
  PX4Platform
    { px4platform_gps_device     :: UART s
    , px4platform_mpu6000_device :: SPIDevice s
    , px4platform_hmc5883_device :: HMC5883Device s
    , px4platform_ms5611_device  :: MS5611Device s
    , px4platform_testplatform   :: BSP.TestPlatform s
    }

px4platform_stm32config :: PX4Platform s -> STM32Config
px4platform_stm32config = BSP.testplatform_stm32 . px4platform_testplatform

data HMC5883Device s =
  HMC5883Device
    { hmc5883device_periph :: I2CPeriph s
    , hmc5883device_sda    :: GPIOPin
    , hmc5883device_scl    :: GPIOPin
    , hmc5883device_addr   :: I2CDeviceAddr
    }

data MS5611Device s =
  MS5611Device
    { ms5611device_periph :: I2CPeriph s
    , ms5611device_sda    :: GPIOPin
    , ms5611device_scl    :: GPIOPin
    , ms5611device_addr   :: I2CDeviceAddr
    }

px4PlatformParser :: ConfigParser (PX4Platform F405.Interrupt)
px4PlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17"      -> return px4fmuv17
    "PX4FMUV17_IOAR" -> return px4fmuv17_ioar
    _ -> fail ("no such platform " ++ p)

px4fmuv17 :: PX4Platform F405.Interrupt
px4fmuv17 = PX4Platform
  { px4platform_gps_device     = F405.uart6
  , px4platform_mpu6000_device = mpu6000
  , px4platform_hmc5883_device = hmc5883
  , px4platform_ms5611_device  = ms5611
  , px4platform_testplatform   = BSP.px4fmuv17
  }
  where
  mpu6000 :: SPIDevice F405.Interrupt
  mpu6000 = SPIDevice
    { spiDevPeripheral    = F405.spi1
    , spiDevCSPin         = F405.pinB0
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "mpu6k"
    }
  hmc5883 :: HMC5883Device F405.Interrupt
  hmc5883 = HMC5883Device
    { hmc5883device_periph = F405.i2c2
    , hmc5883device_sda    = F405.pinB10
    , hmc5883device_scl    = F405.pinB11
    , hmc5883device_addr   = I2CDeviceAddr 0x1e
    }
  ms5611 :: MS5611Device F405.Interrupt
  ms5611 = MS5611Device
    { ms5611device_periph = F405.i2c2
    , ms5611device_sda    = F405.pinB10
    , ms5611device_scl    = F405.pinB11
    , ms5611device_addr   = I2CDeviceAddr 0x76
    }

px4fmuv17_ioar :: PX4Platform F405.Interrupt
px4fmuv17_ioar = px4fmuv17 { px4platform_testplatform = BSP.px4fmuv17_ioar }
