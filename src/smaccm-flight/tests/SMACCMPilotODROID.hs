module Main where

import Ivory.Tower.Config
import Tower.AADL

import SMACCMPilot.Datalink.Mode
import Tower.Odroid.CameraVM (cameraVMTower)
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = compileTowerAADL fst p (vm >> app snd)
  where
  p topts = getConfig topts $ do
    c <- aadlConfigParser $ defaultAADLConfig { configSystemHW = ODROID }
    k <- datalinkModeParser DatalinkServer
    return (c,k)

  vm = do
   _rx <- cameraVMTower
   return ()


