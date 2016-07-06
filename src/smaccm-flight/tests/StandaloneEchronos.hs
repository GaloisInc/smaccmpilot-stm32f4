
module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Standalone (app)

import Tower.AADL
import Tower.AADL.Build.Common
import Tower.AADL.Build.EChronos

import Ivory.Tower.Opts.LockCoarsening
import Ivory.HW (hw_moduledef)
import Ivory.Language.Module (package)
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax

main :: IO ()
main = compileTowerAADLForPlatformWithOpts f p (app id) [lockCoarsening 200 60 unsafeList]
--main = compileTowerAADLForPlatformWithOpts f p (app id) []
--DEPRECATED main = compileTowerSTM32FreeRTOS fp_stm32config p (app id)
  where
  f :: FlightPlatform -> (AADLConfig, OSSpecific STM32Config)
  f config = ( defaultAADLConfig { configSystemOS = EChronos
                                 , configSystemHW = PIXHAWK
                                 }
             , defaultEChronosOS (fp_stm32config config)
             )
  p :: TOpts -> IO FlightPlatform
  p topts = fmap fst (getConfig' topts flightPlatformParser)
  unsafeList :: [Sym]
  unsafeList = (map importSym $ modImports $ package "" hw_moduledef)

  --DEPRECATED p topts = getConfig topts flightPlatformParser
