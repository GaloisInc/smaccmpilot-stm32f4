module Main where

import Ivory.Tower.Config
import Tower.Mini

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = compileTowerMini fst p (app snd False)
  where
  p topts = fmap fst $ getConfig' topts $ do
    k <- datalinkModeParser DatalinkServer
    return (defaultMiniConfig, k)
