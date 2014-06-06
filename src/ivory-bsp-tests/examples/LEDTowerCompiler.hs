{-# LANGUAGE ScopedTypeVariables #-}

-- Compiler imports:
import           Ivory.Language
import           Ivory.Tower.Frontend
import           Ivory.BSP.STM32.PlatformClock
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

-- App imports:
import Ivory.Tower
import LEDTower (blinkApp)
import Platforms

app :: forall i p . (PlatformClock p, ColoredLEDs p, BoardInitializer i p)
    => Tower p ()
app = do
  boardInitializer
  blinkApp period leds
  where
  period = 250
  leds = [redLED p, blueLED p]
  p = (Proxy :: Proxy p)

main :: IO ()
main = compilePlatforms conf (testPlatforms app)
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


