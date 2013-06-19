{-# LANGUAGE DataKinds #-}

module UARTTower where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified LEDTower

import Ivory.BSP.STM32F4.GPIO

app :: Tower ()
app = do
  LEDTower.blinkApp period leds
  where
  period = 333
  -- On PX4FMU 1.x, these are the blue and red leds:
  leds = [pinB14, pinB15]

