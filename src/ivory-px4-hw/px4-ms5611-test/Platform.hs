{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.SPI
import Ivory.BSP.STM32F405.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt as F405

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt
stm32SignalableInstance ''Open407VC     ''F405.Interrupt

class MPU6kPlatform p where
  consoleUart :: Proxy p -> UART F405.Interrupt
  mpu6000Device :: Proxy p -> SPIDevice F405.Interrupt


fmu17MPU6k :: SPIDevice F405.Interrupt
fmu17MPU6k = SPIDevice
  { spiDevPeripheral    = spi1
  , spiDevCSPin         = pinB0
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  , spiDevName          = "mpu6k"
  }

instance PlatformClock PX4FMU17_IOAR where
  platformClockConfig _ = f405ExtXtalMHz 24
instance MPU6kPlatform PX4FMU17_IOAR where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance PlatformClock PX4FMU17_Bare where
  platformClockConfig _ = f405ExtXtalMHz 24
instance MPU6kPlatform PX4FMU17_Bare where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance PlatformClock Open407VC where
  platformClockConfig _ = f405ExtXtalMHz 8
instance MPU6kPlatform Open407VC where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k -- XXX debug device?

gpsPlatforms :: ( forall p . (MPU6kPlatform p, PlatformClock p
                , STM32Signal p, InterruptType p ~ F405.Interrupt)
                  => Tower p ())
             -> [(String, Twr)]
gpsPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ]
