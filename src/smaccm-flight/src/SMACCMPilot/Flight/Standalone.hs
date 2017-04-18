

module SMACCMPilot.Flight.Standalone
  ( app
  ) where

import           Ivory.Tower

import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Flight.Datalink
import           SMACCMPilot.Flight.Datalink.ControllableVehicle
import           SMACCMPilot.Flight.IO
import           SMACCMPilot.Flight.Sensors
import           SMACCMPilot.Flight.Control
import           SMACCMPilot.Flight.Motors
import           SMACCMPilot.Flight.Tuning
import           SMACCMPilot.Flight.Law
import           SMACCMPilot.Flight.Light
import           SMACCMPilot.Flight.PackedStatus
import           SMACCMPilot.Flight.Battery

import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  cvapi@(attrs, _streams) <- controllableVehicleAPI

  mon1 <- flightDatalinks tofp cvapi

  lightTower tofp attrs

  flightTuningTower (fp_tuning . tofp) attrs

  rcin_ui <- channel
  rcin_cm <- channel
  rcin_am <- channel

  flightIOTower tofp attrs
                (fst rcin_ui)
                (fst rcin_cm)
                (fst rcin_am)
                (attrReaderChan (controlLaw attrs))
                (attrReaderChan (motorOutput attrs))


  let lawInputs = LawInputs
        { lawinput_rcinput_arming   = snd rcin_am
        , lawinput_rcinput_ui       = snd rcin_ui
        , lawinput_rcinput_modes    = snd rcin_cm
        , lawinput_telem_arming     = attrReaderChan (armingRequest attrs)
        , lawinput_telem_ui         = attrReaderChan (userInputRequest attrs)
        , lawinput_telem_modes      = attrReaderChan (controlModesRequest attrs)
        , lawinput_px4io_state      = attrReaderChan (px4ioState attrs)
        , lawinput_sensors_output   = attrReaderChan (sensorsOutput attrs)
        }

  control_law       <- channel
  user_input_result <- channel
  arming_status     <- channel
  lawTower lawInputs (fst arming_status) (fst control_law) (fst user_input_result)

  attrProxy (armingStatus attrs) (snd arming_status)
  attrProxy (controlLaw attrs)   (snd control_law)
  attrProxy (userInput attrs)    (snd user_input_result)

  mon2 <- sensorTower tofp attrs
  monitor "dma_uart" (mon1 >> mon2)

  controlTower attrs

  motorMixer (fp_mixer . tofp)
             (controlOutput attrs)
             (controlLaw attrs)
             (motorOutput attrs)

  packedStatusTower attrs

  batteryTower tofp attrs
