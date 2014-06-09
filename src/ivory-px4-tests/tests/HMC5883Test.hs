
module Main where


import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

import PX4.Tests.Platforms
import PX4.Tests.HMC5883L

main :: IO ()
main = compilePlatforms conf (testPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

