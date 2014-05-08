{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.SPI.Peripheral
import Ivory.BSP.STM32F4.Signalable

f24MHz :: Uint32
f24MHz = 24000000
f8MHz :: Uint32
f8MHz = 8000000

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC

stm32f4SignalableInstance ''PX4FMU17_IOAR
stm32f4SignalableInstance ''PX4FMU17_Bare
stm32f4SignalableInstance ''Open407VC

class MPU6kPlatform p where
  consoleUart :: Proxy p -> UART
  mpu6000Device :: Proxy p -> SPIDevice


fmu17MPU6k :: SPIDevice
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
  hseFreq _ = f24MHz
instance MPU6kPlatform PX4FMU17_IOAR where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance BoardHSE PX4FMU17_Bare where
  hseFreq _ = f24MHz
instance MPU6kPlatform PX4FMU17_Bare where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k

instance BoardHSE Open407VC where
  hseFreq _ = f8MHz
instance MPU6kPlatform Open407VC where
  consoleUart _ = uart1
  mpu6000Device _ = fmu17MPU6k -- XXX debug device?

gpsPlatforms :: (forall p . (MPU6kPlatform p, BoardHSE p, STM32F4Signal p)
                  => Tower p ())
             -> [(String, Twr)]
gpsPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ]
