
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.UserInput.TestApp (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS fp_stm32config p (app id)
  where p topts = getConfig topts flightPlatformParser
