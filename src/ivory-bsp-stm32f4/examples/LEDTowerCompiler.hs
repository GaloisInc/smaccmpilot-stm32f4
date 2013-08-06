
-- Compiler imports:
import Ivory.Tower.Frontend
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

-- App imports:
import Ivory.Tower
import Ivory.BSP.STM32F4.GPIO
import LEDTower (blinkApp)

app :: Tower p ()
app = blinkApp period leds
  where
  period = 250
  -- On PX4FMU 1.x, these are the blue and red leds:
  leds = [pinB14, pinB15]

main = compile conf app
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


