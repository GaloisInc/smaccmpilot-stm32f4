module Main where

import SMACCMPilot.Commsec.Ivory.Artifacts
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)
import System.Environment
import Tower.AADL
import Tower.Odroid.UART
import Tower.Odroid.CAN (canArtifacts)

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts c app
  where
  c = addAadlArtifacts (commsecArtifacts ++ canArtifacts) uartConfig
