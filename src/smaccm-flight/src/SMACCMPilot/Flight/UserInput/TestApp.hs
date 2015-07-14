

module SMACCMPilot.Flight.UserInput.TestApp
  ( app
  ) where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Flight.Datalink
import           SMACCMPilot.Flight.UserInput

import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult as S
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- datalinkTower tofp

  -- XXX need to fill in tower component that creates ppm_ui and ppm_cl
  ppm_ui <- channel
  ppm_cl <- channel

  userInputTower (snd ppm_ui) (snd ppm_cl) attrs
  -- Just for testing, lets pretend there is a valid IMU.
  monitor "fake_sensor_output_valid" $ do
    handler systemInit "init" $ do
      e <- attrEmitter (sensorsOutput attrs)
      callback $ const $ do
        v <- local izero
        store (v ~> S.valid) true
        emit e (constRef v)


