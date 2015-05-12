module Main where

import Ivory.Tower.Config
import Tower.AADL

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID (app)

main :: IO ()
main = compileTowerAADL fst p (app snd)
  where
  p topts = getConfig topts $ do
    c <- aadlConfigParser defaultAADLConfig
    k <- datalinkModeParser
    return (c,k)
