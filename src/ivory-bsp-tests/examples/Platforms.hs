{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Platforms where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import LEDTower
import Ivory.BSP.STM32F405.GPIO
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import Ivory.BSP.STM32F405.ClockConfig
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable

class ColoredLEDs p where
  redLED  :: Proxy p -> LED
  blueLED :: Proxy p -> LED

---------- PX4FMUv17 ----------------------------------------------------------
data PX4FMUv17 = PX4FMUv17

instance ColoredLEDs PX4FMUv17 where
  redLED _  = LED pinB14 ActiveLow
  blueLED _ = LED pinB15 ActiveLow

stm32SignalableInstance ''PX4FMUv17 ''F405.Interrupt

instance PlatformClock PX4FMUv17 where
  platformClockConfig _ = f405ExtXtalMHz 24

---------- PX4FMUv24 ----------------------------------------------------------
data PX4FMUv24 = PX4FMUv24

instance ColoredLEDs PX4FMUv24 where
  redLED _  = LED pinE12 ActiveLow
  blueLED _ = LED pinC1  ActiveLow -- DOES NOT EXIST. pinC1 is unassigned.

stm32SignalableInstance ''PX4FMUv24 ''F405.Interrupt

instance PlatformClock PX4FMUv24 where
  platformClockConfig _ = f405ExtXtalMHz 24

---------- F4Discovery --------------------------------------------------------
data F4Discovery = F4Discovery

stm32SignalableInstance ''F4Discovery ''F405.Interrupt

instance ColoredLEDs F4Discovery where
  redLED _  = LED pinD14 ActiveHigh
  blueLED _ = LED pinD15 ActiveHigh

instance PlatformClock F4Discovery where
  platformClockConfig _ = f405ExtXtalMHz 8

---------- Open407VC ----------------------------------------------------------
data Open407VC = Open407VC

stm32SignalableInstance ''Open407VC ''F405.Interrupt

instance ColoredLEDs Open407VC where
  redLED _  = LED pinD12 ActiveHigh
  blueLED _ = LED pinD13 ActiveHigh

instance PlatformClock Open407VC where
  platformClockConfig _ = f405ExtXtalMHz 8


--------- Platform lookup by name ---------------------------------------------

-- XXX fix interrupt polymorphism later
coloredLEDPlatforms :: (forall p . (ColoredLEDs p, PlatformClock p, STM32Signal F405.Interrupt p)
                    => Tower p ()) -> [(String, Twr)]
coloredLEDPlatforms app =
    [("px4fmu17_bare",     Twr (app :: Tower PX4FMUv17 ()))
    ,("px4fmu17_ioar",     Twr (app :: Tower PX4FMUv17 ()))
    ,("stm32f4discovery",  Twr (app :: Tower F4Discovery ()))
    ,("open407vc",         Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",          Twr (app :: Tower PX4FMUv24 ()))
    ]

