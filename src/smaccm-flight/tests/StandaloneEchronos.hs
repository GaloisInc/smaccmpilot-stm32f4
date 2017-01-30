
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
  where
  f :: FlightPlatform -> (AADLConfig, OSSpecific STM32Config)
  f config = ( defaultAADLConfig { configSystemOS     = EChronos
                                 , configSystemHW     = PIXHAWK
                                 , configSystemAddr   = Just 0x8004000
                                 , configRamsesPath   = Just "../../../../ramses-demo"
--                                 , configRamsesPath   = Just "../../../phase2/ramses-demo"
                                 , configEchronosPath = Just "../../../echronos"
                                 }
             , defaultEChronosOS (fp_stm32config config)
             )
  p :: TOpts -> IO FlightPlatform
  p topts = fmap fst (getConfig' topts flightPlatformParser)

