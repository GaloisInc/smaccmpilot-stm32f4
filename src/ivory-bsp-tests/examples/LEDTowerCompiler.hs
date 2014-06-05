{-# LANGUAGE ScopedTypeVariables #-}

-- Compiler imports:
import           Ivory.Language
import           Ivory.Tower.Frontend
import           Ivory.BSP.STM32.PlatformClock
import           Ivory.BSP.STM32F405.Init
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

-- App imports:
import Ivory.Tower
import LEDTower (blinkApp)
import Platforms

app :: forall p . (PlatformClock p, ColoredLEDs p) => Tower p ()
app = do
  stm32f405InitTower
  blinkApp period leds
  where
  period = 250
  leds = [redLED p, blueLED p]
  p = (Proxy :: Proxy p)

main :: IO ()
main = compilePlatforms conf (testPlatforms app)
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


