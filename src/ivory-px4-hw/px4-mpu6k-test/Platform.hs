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

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.SPI.Peripheral
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import Ivory.BSP.STM32.Signalable

f24MHz :: Integer
f24MHz = 24000000
f8MHz :: Integer
f8MHz = 8000000

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC
data PX4FMU24      = PX4FMU24

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt
stm32SignalableInstance ''Open407VC     ''F405.Interrupt
stm32SignalableInstance ''PX4FMU24      ''F405.Interrupt

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

instance BoardHSE PX4FMU17_IOAR where
  hseFreqHz _ = f24MHz
instance MPU6kPlatform PX4FMU17_IOAR where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance BoardHSE PX4FMU17_Bare where
  hseFreqHz _ = f24MHz
instance MPU6kPlatform PX4FMU17_Bare where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance BoardHSE Open407VC where
  hseFreqHz _ = f8MHz
instance MPU6kPlatform Open407VC where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k -- XXX debug device?

instance BoardHSE PX4FMU24 where
  hseFreqHz _ = f24MHz
instance MPU6kPlatform PX4FMU24 where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k -- XXX FIXME

gpsPlatforms :: (forall p
                   . (MPU6kPlatform p, BoardHSE p, STM32Signal F405.Interrupt p)
                  => Tower p ())
             -> [(String, Twr)]
gpsPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",      Twr (app :: Tower PX4FMU24 ()))
    ]
