{-# LANGUAGE Rank2Types #-}

module Platforms where

import Ivory.Tower
import Ivory.Tower.Frontend

import LEDTower
import Ivory.BSP.STM32F4.GPIO

data PX4FMUv17 = PX4FMUv17
data F4Discovery = F4Discovery
data Open407VC = Open407VC

class ColoredLEDs p where
  redLED  :: p -> LED
  blueLED :: p -> LED

instance ColoredLEDs PX4FMUv17 where
  redLED _  = LED pinB14 ActiveLow
  blueLED _ = LED pinB15 ActiveLow

instance ColoredLEDs F4Discovery where
  redLED _  = LED pinD14 ActiveHigh
  blueLED _ = LED pinD15 ActiveHigh

instance ColoredLEDs Open407VC where
  redLED _  = LED pinD12 ActiveHigh
  blueLED _ = LED pinD13 ActiveHigh

coloredLEDPlatforms :: (forall p . (ColoredLEDs p) => Tower p ()) -> [(String, Twr)]
coloredLEDPlatforms app =
    [("px4fmu17",          Twr (app :: Tower PX4FMUv17 ()))
    ,("stm32f4discovery",  Twr (app :: Tower F4Discovery ()))
    ,("open407vc",         Twr (app :: Tower Open407VC ()))
    ]
