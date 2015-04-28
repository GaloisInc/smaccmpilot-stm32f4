
module SMACCMPilot.Flight.Datalink.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.Plaintext


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  fp <- fmap tofp getEnv
  let telem_uart = fp_telem fp

  _ <- plaintextDatalink tocc telem_uart 115200 controllableVehicle

  return ()
  where
  tocc = fp_clockconfig . tofp
