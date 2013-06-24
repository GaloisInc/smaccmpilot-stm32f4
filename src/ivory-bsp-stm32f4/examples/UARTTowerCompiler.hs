
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Tower.Graphviz

import qualified Ivory.Tower.Compile.FreeRTOS as FreeRTOS
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import UARTTower (app)


main = do
  let (asm, objs) = FreeRTOS.compile app
  compileWith Nothing (Just [FreeRTOS.searchDir, HW.searchDir, BSP.searchDir]) objs
  graphvizToFile "uarttower.dot" asm


