{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law where

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput  as I ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as CL
import qualified SMACCMPilot.Comm.Ivory.Types.RcInput    as RC
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate   as T

import           SMACCMPilot.Flight.Law.Arming

data LawInputs =
  LawInputs
    { lawinput_rcinput_arming   :: ChanOutput (Stored T.Tristate)
    , lawinput_rcinput_ui       :: ChanOutput (Struct "user_input") -- CLOCK FOR user_input_result OUTPUT
    , lawinput_rcinput_modes    :: ChanOutput (Struct "control_modes") -- CLOCK FOR control_law OUTPUT
    , lawinput_telem_arming     :: ChanOutput (Stored T.Tristate)
    , lawinput_telem_ui         :: ChanOutput (Struct "user_input")
    , lawinput_telem_modes      :: ChanOutput (Struct "control_modes")
    , lawinput_px4io_state      :: ChanOutput (Struct "px4io_state")
    , lawinput_gyro_cal_output  :: ChanOutput (Struct "xyz_calibration")
    , lawinput_accel_cal_output :: ChanOutput (Struct "xyz_calibration")
    , lawinput_mag_cal_output   :: ChanOutput (Struct "xyz_calibration")
    , lawinput_sensors_output   :: ChanOutput (Struct "sensors_result")
    }

lawTower :: LawInputs
         -> ChanInput (Struct "control_law")
         -> ChanInput (Struct "user_input_result")
         -> Tower e ()
lawTower LawInputs{..} law_output ui_output = do

  -- TODO: construct SomeArmingInput from:
  --  * _rcinput_arming (use as clock)
  --  * _telem_arming
  --  * _px4io_state
  --  * _gyro_cal_output
  --  * _accel_cal_output
  --  * _mag_cal_output
  --  * _sensors_output
  let rcinput_arming_input = SomeArmingInput $ ArmingInput
        { ai_name = "rcinput"
        , ai_chan = lawinput_rcinput_arming
        , ai_get  = deref
        }

  arming_mode <- channel
  armingTower rcinput_arming_input [] (fst arming_mode)

  monitor "control_law" $ do
    am <- state "arming_mode_"
    handler (snd arming_mode) "armingTower_arming_mode" $ do
      callback $ \am' -> refCopy am am'

    handler lawinput_rcinput_modes "rcinput_control_modes" $ do
      e <- emitter law_output 1
      callback $ \cm -> do
        law <- local izero
        refCopy (law ~> CL.arming_mode) am
        refCopy (law ~> CL.control_modes) cm
        emit e (constRef law)


