

module SMACCMPilot.Flight.Standalone
  ( app
  ) where

import           Ivory.Tower

import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Flight.Datalink
import           SMACCMPilot.Flight.IO
import           SMACCMPilot.Flight.UserInput
import           SMACCMPilot.Flight.Sensors
import           SMACCMPilot.Flight.Control
import           SMACCMPilot.Flight.Motors
import           SMACCMPilot.Flight.Tuning

import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- datalinkTower tofp 57600

  flightTuningTower (fp_tuning . tofp) attrs

  rcin_ui <- channel
  rcin_cl <- channel

  flightIOTower tofp attrs
                (fst rcin_ui) (fst rcin_cl)
                (attrReaderChan (controlLaw attrs))
                (attrReaderChan (motorOutput attrs))

  userInputTower (snd rcin_ui) (snd rcin_cl) attrs

  sensorTower tofp attrs

  controlTower attrs

  motorMixer (controlOutput attrs)
             (controlLaw attrs)
             (motorOutput attrs)
