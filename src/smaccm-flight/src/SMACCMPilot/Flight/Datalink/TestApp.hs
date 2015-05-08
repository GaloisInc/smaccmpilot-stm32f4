
module SMACCMPilot.Flight.Datalink.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  _ <- datalinkTower tofp
  return ()
