

module SMACCMPilot.Flight.Sensors.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.Sensors
import SMACCMPilot.Flight.IO

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- datalinkTower tofp 115200
  sensorTower tofp attrs

  -- flightIOTower just exists to populate the px4ioState attr, so we
  -- can trigger accel cal.
  ui <- channel
  cm <- channel
  am <- channel
  output_cl <- channel
  output_motors <- channel

  flightIOTower tofp attrs (fst ui) (fst cm) (fst am)
                           (snd output_cl) (snd output_motors)
