

module SMACCMPilot.Flight.Sensors.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.Datalink.UART
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Sensors
import SMACCMPilot.Flight.IO

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  cvapi@(attrs, _streams) <- controllableVehicleAPI

  fp   <- fmap tofp getEnv
  mon1 <- datalinkTower tofp cvapi
    (uartDatalink (fp_clockconfig . tofp) (fp_telem fp) 115200)

  mon2 <- sensorTower tofp attrs
  monitor "uart_dma" (mon1 >> mon2)

  -- flightIOTower just exists to populate the px4ioState attr, so we
  -- can trigger accel cal.
  ui <- channel
  cm <- channel
  am <- channel
  output_cl <- channel
  output_motors <- channel

  flightIOTower tofp attrs (fst ui) (fst cm) (fst am)
                           (snd output_cl) (snd output_motors)
