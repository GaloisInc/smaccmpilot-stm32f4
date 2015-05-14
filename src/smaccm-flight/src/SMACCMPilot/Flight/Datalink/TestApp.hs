
module SMACCMPilot.Flight.Datalink.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.Light


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- datalinkTower tofp
  lightTower tofp attrs
