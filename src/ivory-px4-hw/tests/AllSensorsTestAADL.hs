
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options

import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Tests.AllSensors (app)

import Tower.AADL
import Tower.AADL.Build.Common
import Tower.AADL.Build.EChronos

main :: IO ()
-- main = compileTowerSTM32FreeRTOS px4platform_stm32config p (app id)
--   where p topts = getConfig topts px4PlatformParser

main = compileTowerAADLForPlatform f p $ do
  app id
  where
  f :: PX4Platform -> (AADLConfig, OSSpecific STM32Config)
  f tp = ( defaultAADLConfig { configSystemOS  = EChronos
                             , configSystemHW  = PIXHAWK
                             , configRamsesPath = Just "../../../../ramses-demo"
                             , configEchronosPath = Just "../../../echronos"
                             }
         , defaultEChronosOS (px4platform_stm32config tp)
         )
  p :: TOpts -> IO PX4Platform
  p topts = fmap fst (getConfig' topts px4PlatformParser)
