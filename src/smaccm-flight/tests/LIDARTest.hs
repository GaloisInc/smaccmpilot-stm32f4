-- Test the LIDARLite peripheral by having it feed data over the
-- UART. To read it, use the test-client python script in ivory-px4-hw.
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Hardware.Platforms

import LIDAR.LIDARApp (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS (fp_stm32config . fst) p (app fst snd)
  where p topts = do
          fp  <- getConfig topts flightPlatformParser
          px4 <- getConfig topts px4PlatformParser
          return (fp, px4)
