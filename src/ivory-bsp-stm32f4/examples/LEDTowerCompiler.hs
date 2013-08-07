{-# LANGUAGE ScopedTypeVariables #-}

-- Compiler imports:
import Ivory.Tower.Frontend
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

-- App imports:
import Ivory.Tower
import Ivory.BSP.STM32F4.GPIO
import LEDTower (blinkApp)

app :: forall p . (ColoredLEDs p) => Tower p ()
app = blinkApp period leds
  where
  period = 250
  -- On PX4FMU 1.x, these are the blue and red leds:
  leds = [redLED p, blueLED p]
  p = (undefined :: p) -- ugly, is there a better way?

data PX4FMUv17 = PX4FMUv17
data OpenF407 = OpenF407

class ColoredLEDs p where
  redLED  :: p -> GPIOPin
  blueLED :: p -> GPIOPin

instance ColoredLEDs PX4FMUv17 where
  redLED _  = pinB14
  blueLED _ = pinB15

instance ColoredLEDs OpenF407 where -- this is just made up
  redLED _  = pinA14
  blueLED _ = pinA15

main = compilePlatforms conf platforms
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]
  platforms =
    [("px4fmu17", Twr (app :: Tower PX4FMUv17 ()))
    ,("open407",  Twr (app :: Tower OpenF407 ()))]


