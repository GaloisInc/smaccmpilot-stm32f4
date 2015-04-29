

module SMACCMPilot.Flight.UserInput.TestApp
  ( app
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.Plaintext
import SMACCMPilot.Flight.UserInput

import SMACCMPilot.Comm.Tower.Attr
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

test_harness :: (e -> FlightPlatform)
             -> Tower e ( ControllableVehicleAttrs Attr
                        , ControllableVehicleStreams ChanInput)
test_harness tofp = do
  fp <- fmap tofp getEnv
  let telem_uart = fp_telem fp
  plaintextDatalink tocc telem_uart 115200 controllableVehicle
  where
  tocc = fp_clockconfig . tofp

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- test_harness tofp
  userInputTower tofp attrs
