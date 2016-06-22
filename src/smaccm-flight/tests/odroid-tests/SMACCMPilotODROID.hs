module Main where

import Ivory.Tower.Config
import Tower.AADL

import SMACCMPilot.Datalink.Mode
import Tower.Odroid.CameraVM
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = do
  compileTowerAADL fst p (do rx <- cameraVMTower
                             app snd (Just rx)
                         )
  where
  p topts = fmap fst $ getConfig' topts $ do
    k <- datalinkModeParser DatalinkServer
    return (cameraVMConfig,k)
