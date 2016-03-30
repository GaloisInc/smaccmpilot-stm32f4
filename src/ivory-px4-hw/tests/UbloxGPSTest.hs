
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Tests.Ublox (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS px4platform_stm32config p (app id)
  where p topts = getConfig topts px4PlatformParser
