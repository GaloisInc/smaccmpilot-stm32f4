
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Standalone (app)
import Ivory.Tower.Opts.LockCoarsening
import Ivory.HW (hw_moduledef)
import Ivory.Language.Module (package)
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax

main :: IO ()
main = compileTowerSTM32FreeRTOSWithOpts fp_stm32config p (app id) [lockCoarsening 200 60 unsafeList]
--main = compileTowerSTM32FreeRTOSWithOpts fp_stm32config p (app id) []
  where 
    p topts = getConfig topts flightPlatformParser
    unsafeList :: [Sym]
    unsafeList = (map importSym $ modImports $ package "" hw_moduledef)
