
module Main where

import Ivory.Tower.Frontend
import PX4.Tests.CopterMotors
import PX4.Tests.Platforms

import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

main :: IO ()
main = compilePlatforms conf (motorPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]
