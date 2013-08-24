
module Main where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import SMACCMPilot.Hardware.PX4IOAR

main :: IO ()
main = compile conf app
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: Tower p ()
app = do
  c <- channel
  px4ioarTower (snk c)

