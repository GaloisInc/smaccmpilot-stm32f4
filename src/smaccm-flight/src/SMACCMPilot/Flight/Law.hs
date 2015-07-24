{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law where

import           Ivory.Language
import           Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw     as CL
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate       as T
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioState     as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioStatus    as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration as Cal
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult  as S

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
lawTower LawInputs{..} law_output _ui_output = do

  -- TODO: construct SomeArmingInput from:
  --  * _gyro_cal_output
  --  * _accel_cal_output
  --  * _mag_cal_output
  --  * _sensors_output
  let rcinput_arming_input = SomeArmingInput $ ArmingInput
        { ai_name = "rcinput"
        , ai_chan = lawinput_rcinput_arming
        , ai_get  = deref
        }
      telem_arming_input = SomeArmingInput $ ArmingInput
        { ai_name = "telem"
        , ai_chan = lawinput_telem_arming
        , ai_get  = deref
        }
      px4io_state_input = SomeArmingInput $ ArmingInput
        { ai_name = "px4io"
        , ai_chan = lawinput_px4io_state
        , ai_get  = arming_from_px4io_state
        }
      gyro_cal_input = SomeArmingInput $ ArmingInput
        { ai_name = "gyro_cal"
        , ai_chan = lawinput_gyro_cal_output
        , ai_get  = arming_from_xyzcal
        }
      accel_cal_input = SomeArmingInput $ ArmingInput
        { ai_name = "accel_cal"
        , ai_chan = lawinput_accel_cal_output
        , ai_get  = arming_from_xyzcal
        }
      mag_cal_input = SomeArmingInput $ ArmingInput
        { ai_name = "mag_cal"
        , ai_chan = lawinput_mag_cal_output
        , ai_get  = arming_from_xyzcal
        }
      sens_cal_input = SomeArmingInput $ ArmingInput
        { ai_name = "sens_cal"
        , ai_chan = lawinput_sensors_output
        , ai_get  = arming_from_sensors
        }

  arming_mode <- channel
  armingTower rcinput_arming_input
    [ telem_arming_input
    , px4io_state_input
    , gyro_cal_input
    , accel_cal_input
    , mag_cal_input
    , sens_cal_input
    ]
    (fst arming_mode)

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

  where
  arming_from_px4io_state s = do
    safety_off <- deref (s ~> PX4.status ~> PX4.safety_off)
    return (safety_off ? (T.neutral, T.negative))

  arming_from_xyzcal cal = do
    v <- deref (cal ~> Cal.valid)
    return (v ? (T.neutral, T.negative))

  arming_from_sensors sens = do
    v <- deref (sens ~> S.valid)
    return (v ? (T.neutral, T.negative))
