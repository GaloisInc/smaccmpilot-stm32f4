{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.Platforms where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import           Ivory.BSP.STM32F405.Init
import           Ivory.BSP.STM32F405.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable

import BSP.Tests.LED

class ColoredLEDs p where
  redLED  :: Proxy p -> LED
  blueLED :: Proxy p -> LED

class (PlatformClock p, STM32Signal p) => BoardInitializer p where
  boardInitializer :: Tower p ()

class (STM32Signal p) => TestUART p where
  testUART :: Proxy p -> UART (InterruptType p)

class (STM32Signal p) => TestSPI p where
  testSPI :: Proxy p -> SPIPeriph (InterruptType p)

class (STM32Signal p) => TestI2C p where
  testI2C :: Proxy p -> I2CPeriph (InterruptType p)
  testSDA :: Proxy p -> GPIOPin
  testSCL :: Proxy p -> GPIOPin

class (STM32Signal p) => TestCAN p where
  testCAN :: Proxy p -> CANPeriph (InterruptType p)
  testCANRX :: Proxy p -> GPIOPin
  testCANTX :: Proxy p -> GPIOPin
  testCANFilters :: Proxy p -> CANPeriphFilters

---------- PX4FMUv17 ----------------------------------------------------------
data PX4FMUv17 = PX4FMUv17

instance ColoredLEDs PX4FMUv17 where
  redLED _  = LED F405.pinB14 ActiveLow
  blueLED _ = LED F405.pinB15 ActiveLow

stm32SignalableInstance ''PX4FMUv17 ''F405.Interrupt

instance PlatformClock PX4FMUv17 where
  platformClockConfig _ = f405ExtXtalMHz 24

instance BoardInitializer PX4FMUv17 where
  boardInitializer = stm32f405InitTower

instance TestUART PX4FMUv17 where
  testUART _ = F405.uart5

instance TestSPI PX4FMUv17 where
  testSPI _ = F405.spi3

instance TestI2C PX4FMUv17 where
  testI2C _ = F405.i2c1
  testSDA _ = F405.pinB6
  testSCL _ = F405.pinB7

instance TestCAN PX4FMUv17 where
  testCAN _ = F405.can1
  testCANRX _ = F405.pinD0
  testCANTX _ = F405.pinD1
  testCANFilters _ = F405.canFilters

---------- PX4FMUv24 ----------------------------------------------------------
data PX4FMUv24 = PX4FMUv24

instance ColoredLEDs PX4FMUv24 where
  redLED _  = LED F405.pinE12 ActiveLow
  blueLED _ = LED F405.pinC1  ActiveLow -- DOES NOT EXIST. pinC1 is unassigned.

stm32SignalableInstance ''PX4FMUv24 ''F405.Interrupt -- XXX FIXME

instance PlatformClock PX4FMUv24 where
  platformClockConfig _ = f405ExtXtalMHz 24

instance BoardInitializer PX4FMUv24 where
  boardInitializer = stm32f405InitTower -- XXX FIXME

instance TestUART PX4FMUv24 where
  testUART _ = F405.uart5 -- XXX FIXME

instance TestSPI PX4FMUv24 where
  testSPI _ = F405.spi3 -- XXX FIXME

instance TestI2C PX4FMUv24 where
  testI2C _ = F405.i2c1 -- XXX FIXME
  testSDA _ = F405.pinB6
  testSCL _ = F405.pinB7

instance TestCAN PX4FMUv24 where
  testCAN _ = F405.can1
  testCANRX _ = F405.pinD0
  testCANTX _ = F405.pinD1
  testCANFilters _ = F405.canFilters


---------- F4Discovery --------------------------------------------------------
data F4Discovery = F4Discovery

stm32SignalableInstance ''F4Discovery ''F405.Interrupt

instance ColoredLEDs F4Discovery where
  redLED _  = LED F405.pinD14 ActiveHigh
  blueLED _ = LED F405.pinD15 ActiveHigh

instance PlatformClock F4Discovery where
  platformClockConfig _ = f405ExtXtalMHz 8

instance BoardInitializer F4Discovery where
  boardInitializer = stm32f405InitTower

instance TestUART F4Discovery where
  testUART _ = F405.uart1

instance TestSPI F4Discovery where
  testSPI _ = F405.spi3

instance TestI2C F4Discovery where
  testI2C _ = F405.i2c1 -- XXX FIXME
  testSDA _ = F405.pinB6
  testSCL _ = F405.pinB7

instance TestCAN F4Discovery where
  testCAN _ = F405.can1
  testCANRX _ = F405.pinD0
  testCANTX _ = F405.pinD1
  testCANFilters _ = F405.canFilters

---------- Open407VC ----------------------------------------------------------
data Open407VC = Open407VC

stm32SignalableInstance ''Open407VC ''F405.Interrupt

instance ColoredLEDs Open407VC where
  redLED _  = LED F405.pinD12 ActiveHigh
  blueLED _ = LED F405.pinD13 ActiveHigh

instance PlatformClock Open407VC where
  platformClockConfig _ = f405ExtXtalMHz 8

instance BoardInitializer Open407VC where
  boardInitializer = stm32f405InitTower

instance TestUART Open407VC where
  testUART _ = F405.uart2

instance TestSPI Open407VC where
  testSPI _ = F405.spi3

instance TestI2C Open407VC where
  testI2C _ = F405.i2c1
  testSDA _ = F405.pinB6
  testSCL _ = F405.pinB7

instance TestCAN Open407VC where
  testCAN _ = F405.can1
  testCANRX _ = F405.pinD0
  testCANTX _ = F405.pinD1
  testCANFilters _ = F405.canFilters

--------- Platform lookup by name ---------------------------------------------

testPlatforms :: (forall p
                  . ( ColoredLEDs p
                    , PlatformClock p
                    , STM32Signal p
                    , BoardInitializer p
                    , TestUART p
                    , TestSPI p
                    , TestI2C p
                    , TestCAN p)
                    => Tower p ()) -> [(String, Twr)]
testPlatforms app =
    [("px4fmu17_bare",     Twr (app :: Tower PX4FMUv17 ()))
    ,("px4fmu17_ioar",     Twr (app :: Tower PX4FMUv17 ()))
    ,("stm32f4discovery",  Twr (app :: Tower F4Discovery ()))
    ,("open407vc",         Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",          Twr (app :: Tower PX4FMUv24 ()))
    ]

