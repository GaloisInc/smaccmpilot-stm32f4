
import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

import Platforms
import SPITest (app)

main :: IO ()
main = compilePlatforms conf (coloredLEDPlatforms app)
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


