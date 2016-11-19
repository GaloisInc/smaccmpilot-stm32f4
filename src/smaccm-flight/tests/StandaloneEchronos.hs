
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Standalone (app)

import Tower.AADL
import Tower.AADL.Build.Common
import Tower.AADL.Build.EChronos

main :: IO ()
main = compileTowerAADLForPlatform f p (app id)
-- main = compileTowerSTM32FreeRTOS fp_stm32config p (app id)
  where
  f :: FlightPlatform -> (AADLConfig, OSSpecific STM32Config)
  f config = ( defaultAADLConfig { configSystemOS   = EChronos
                                 , configSystemHW   = PIXHAWK
                                 , configSystemAddr = Just 0x8004000
                                 , configBuildRoot  = Just "../../../../../"
                                 }
             , defaultEChronosOS (fp_stm32config config)
             )
  p :: TOpts -> IO FlightPlatform
  p topts = fmap fst (getConfig' topts flightPlatformParser)
  -- p topts = getConfig topts flightPlatformParser
