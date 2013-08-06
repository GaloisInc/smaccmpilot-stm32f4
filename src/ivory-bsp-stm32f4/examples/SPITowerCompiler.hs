
import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import SPITower (app)

main = compile conf app
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


