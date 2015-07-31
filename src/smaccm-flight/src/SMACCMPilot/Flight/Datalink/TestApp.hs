
module SMACCMPilot.Flight.Datalink.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.Datalink.UART
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Light


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  cvapi@(attrs, _streams) <- controllableVehicleAPI
  fp <- fmap tofp getEnv
  datalinkTower tofp cvapi (u fp)
  lightTower tofp attrs
  where
  u fp = uartDatalink (fp_clockconfig . tofp) (fp_telem fp) 115200
