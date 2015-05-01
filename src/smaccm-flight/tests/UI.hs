
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.UserInput.TestApp (app)

main :: IO ()
main = towerCompile p (app id)
  where
  p topts = do
    cfg <- getConfig topts flightPlatformParser
    return $ stm32FreeRTOS fp_stm32config cfg
