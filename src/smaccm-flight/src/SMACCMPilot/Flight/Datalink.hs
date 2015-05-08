
module SMACCMPilot.Flight.Datalink
  ( datalinkTower
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.UART
import SMACCMPilot.Flight.Datalink.Commsec
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Comm.Tower.Attr


datalinkTower :: (e -> FlightPlatform)
              -> Tower e (ControllableVehicleAttrs Attr, ControllableVehicleStreams ChanInput)
datalinkTower tofp = do
  fp <- fmap tofp getEnv
  let telem_uart = fp_telem fp

  uartDatalink tocc telem_uart 115200
    $ commsecDatalink todatalink
    $ controllableVehicle

  where
  tocc = fp_clockconfig . tofp
  todatalink = fp_datalink . tofp
