module Main where

import Ivory.Tower.Config
import Tower.AADL

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = compileTowerAADL fst p (app snd)
  where
  p topts = getConfig topts $ do
    c <- aadlConfigParser $ defaultAADLConfig { configSystemHW = ODROID }
    k <- datalinkModeParser DatalinkServer
    return (c,k)
