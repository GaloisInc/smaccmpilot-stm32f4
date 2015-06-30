module Main where

import Ivory.Tower.Config
import Tower.AADL

import SMACCMPilot.Datalink.Mode
import Tower.Odroid.CameraVM (cameraVMTower)
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = do
  compileTowerAADL fst p (do rx <- cameraVMTower
                             app snd (Just rx))
  where
  p topts = getConfig topts $ do
    c <- aadlConfigParser $ defaultAADLConfig { configSystemHW = ODROID }
    k <- datalinkModeParser DatalinkServer
    return (c,k)
