{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PX4.Tests.Platforms where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import qualified SMACCMPilot.Hardware.PX4IOAR as IOAR
import qualified SMACCMPilot.Hardware.PX4FMU17 as Bare

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

import qualified Ivory.BSP.STM32F405.UART as F405
import qualified Ivory.BSP.STM32F405.GPIO as F405
import qualified Ivory.BSP.STM32F405.I2C  as F405
import qualified Ivory.BSP.STM32F405.SPI  as F405
import           Ivory.BSP.STM32F405.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt as F405

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC
data PX4FMU24      = PX4FMU24

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt
stm32SignalableInstance ''Open407VC     ''F405.Interrupt
stm32SignalableInstance ''PX4FMU24      ''F405.Interrupt

class (STM32Signal p, PlatformClock p) => TestPlatform p where
  consoleUart   :: Proxy p -> UART      (InterruptType p)

  gpsUart       :: Proxy p -> UART      (InterruptType p)

  mpu6000Device :: Proxy p -> SPIDevice (InterruptType p)

  hmc5883periph :: Proxy p -> I2CPeriph (InterruptType p)
  hmc5883sda    :: Proxy p -> GPIOPin
  hmc5883scl    :: Proxy p -> GPIOPin
  hmc5883addr   :: Proxy p -> I2CDeviceAddr

  ms5611periph  :: Proxy p -> I2CPeriph (InterruptType p)
  ms5611sda     :: Proxy p -> GPIOPin
  ms5611scl     :: Proxy p -> GPIOPin
  ms5611addr    :: Proxy p -> I2CDeviceAddr

class RawMotorControl p where
  rawMotorControl :: ChannelSink (Array 4 (Stored IFloat)) -> Tower p ()

fmu17MPU6k :: SPIDevice F405.Interrupt
fmu17MPU6k = SPIDevice
  { spiDevPeripheral    = F405.spi1
  , spiDevCSPin         = F405.pinB0
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  , spiDevName          = "mpu6k"
  }

instance PlatformClock PX4FMU17_IOAR where
  platformClockConfig _ = f405ExtXtalMHz 24
instance TestPlatform PX4FMU17_IOAR where
  consoleUart _ = F405.uart1
  mpu6000Device _ = fmu17MPU6k
  gpsUart _ = F405.uart6
  hmc5883periph _ = F405.i2c2
  hmc5883sda _ = F405.pinB10
  hmc5883scl _ = F405.pinB11
  hmc5883addr _ = I2CDeviceAddr 0x1e
  ms5611periph _ = F405.i2c2
  ms5611sda _ = F405.pinB10
  ms5611scl _ = F405.pinB11
  ms5611addr _ = I2CDeviceAddr 0x76

instance RawMotorControl PX4FMU17_IOAR where
  rawMotorControl = IOAR.motorControlTower cpystack

instance PlatformClock PX4FMU17_Bare where
  platformClockConfig _ = f405ExtXtalMHz 24
instance TestPlatform PX4FMU17_Bare where
  consoleUart _ = F405.uart1
  mpu6000Device _ = fmu17MPU6k
  gpsUart _ = F405.uart6
  hmc5883periph _ = F405.i2c2
  hmc5883sda _ = F405.pinB10
  hmc5883scl _ = F405.pinB11
  hmc5883addr _ = I2CDeviceAddr 0x1e
  ms5611periph _ = F405.i2c2
  ms5611sda _ = F405.pinB10
  ms5611scl _ = F405.pinB11
  ms5611addr _ = I2CDeviceAddr 0x76

instance RawMotorControl PX4FMU17_Bare where
  rawMotorControl = Bare.motorControlTower cpystack

instance PlatformClock Open407VC where
  platformClockConfig _ = f405ExtXtalMHz 8
instance TestPlatform Open407VC where
  consoleUart _ = F405.uart1
  mpu6000Device _ = fmu17MPU6k -- XXX debug device?
  gpsUart _ = F405.uart2
  hmc5883periph _ = F405.i2c2
  hmc5883sda _ = F405.pinB10
  hmc5883scl _ = F405.pinB11
  hmc5883addr _ = I2CDeviceAddr 0x1e
  ms5611periph _ = F405.i2c2
  ms5611sda _ = F405.pinB10
  ms5611scl _ = F405.pinB11
  ms5611addr _ = I2CDeviceAddr 0x76

instance PlatformClock PX4FMU24 where
  platformClockConfig _ = f405ExtXtalMHz 24
instance TestPlatform PX4FMU24 where
  consoleUart _ = F405.uart1
  mpu6000Device _ = fmu17MPU6k -- XXX FIXME
  gpsUart _ = F405.uart3
  -- XXX FIXME:
  hmc5883periph _ = F405.i2c2
  hmc5883sda _ = F405.pinB10
  hmc5883scl _ = F405.pinB11
  hmc5883addr _ = I2CDeviceAddr 0x1e
  ms5611periph _ = F405.i2c2
  ms5611sda _ = F405.pinB10
  ms5611scl _ = F405.pinB11
  ms5611addr _ = I2CDeviceAddr 0x76

testPlatforms :: (forall p . (TestPlatform p) => Tower p ())
              -> [(String, Twr)]
testPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",      Twr (app :: Tower PX4FMU24 ()))
    ]

motorPlatforms :: (forall p . (TestPlatform p, RawMotorControl p) => Tower p ())
               -> [(String, Twr)]
motorPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ]

cpystack :: ConstRef s (Array 4 (Stored IFloat))
         -> Ivory (AllocEffects cs)
              (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
cpystack v = do
  l <- local (iarray [])
  arrayMap $ \i -> deref (v ! i) >>= store (l ! i)
  return (constRef l)

