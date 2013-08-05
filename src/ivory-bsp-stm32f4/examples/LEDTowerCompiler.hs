
import Ivory.Tower


import Ivory.Compile.C.CmdlineFrontend
import Ivory.Tower.Graphviz

import qualified Ivory.Tower.Compile.FreeRTOS as FreeRTOS
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.GPIO

import LEDTower (blinkApp)

app :: Tower p ()
app = blinkApp period leds
  where
  period = 250
  -- On PX4FMU 1.x, these are the blue and red leds:
  leds = [pinB14, pinB15]

main = do
  let (asm, objs) = FreeRTOS.compile app
  compileWith Nothing (Just [FreeRTOS.searchDir, HW.searchDir, BSP.searchDir]) objs
  graphvizToFile "ledtower.dot" asm


