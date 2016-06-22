module Main where

import Ivory.Tower.Config
import Tower.AADL
import Tower.Odroid.CAN (canConfig)

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = compileTowerAADL fst p (app snd Nothing)
  where
  p topts = fmap fst $ getConfig' topts $ do
    k <- datalinkModeParser DatalinkServer
    return (canConfig, k)
