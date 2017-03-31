{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law where

import           Ivory.Language
import           Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.ArmingStatus   as A
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode     as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw     as CL
--import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes   as CM
--import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleMode   as TM
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate       as T
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioState     as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioStatus    as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult  as S

import           SMACCMPilot.Flight.Law.Arming
import           SMACCMPilot.Flight.Law.ControlModes
import           SMACCMPilot.Flight.Law.UserInput

data LawInputs =
  LawInputs
    { lawinput_rcinput_arming   :: ChanOutput ('Stored T.Tristate)
    , lawinput_rcinput_ui       :: ChanOutput ('Struct "user_input") -- CLOCK FOR user_input_result OUTPUT
    , lawinput_rcinput_modes    :: ChanOutput ('Struct "control_modes") -- CLOCK FOR control_law OUTPUT
    , lawinput_telem_arming     :: ChanOutput ('Stored T.Tristate)
    , lawinput_telem_ui         :: ChanOutput ('Struct "user_input")
    , lawinput_telem_modes      :: ChanOutput ('Struct "control_modes")
    , lawinput_px4io_state      :: ChanOutput ('Struct "px4io_state")
    , lawinput_sensors_output   :: ChanOutput ('Struct "sensors_result")
    }

lawTower :: LawInputs
         -> ChanInput ('Struct "arming_status")
         -> ChanInput ('Struct "control_law")
         -> ChanInput ('Struct "user_input_result")
         -> Tower e ()
lawTower lis@LawInputs{..} as_output law_output ui_output = do
  arming_mode <- channel
  control_modes <- channel

  armingLawTower lis (fst arming_mode) as_output

  controlModesTowerRCOnly lawinput_rcinput_modes
                          lawinput_telem_modes
                          (fst control_modes)

  userInputMuxTower (snd control_modes)
                    lawinput_rcinput_ui
                    lawinput_telem_ui
                    ui_output

  monitor "control_law" $ do
    am <- state "arming_mode_"
    handler (snd arming_mode) "armingTower_arming_mode" $ do
      callback $ \am' -> refCopy am am'

    handler (snd control_modes) "controlModesTower_control_modes" $ do
      e <- emitter law_output 1
      callback $ \cm -> do
        law <- local izero
        refCopy (law ~> CL.arming_mode) am
        refCopy (law ~> CL.control_modes) cm
        emit e (constRef law)

  where

armingLawTower :: LawInputs
               -> ChanInput ('Stored A.ArmingMode)
               -> ChanInput ('Struct "arming_status")
               -> Tower e ()
armingLawTower LawInputs{..} arming_output arming_status = do
  armingTower rcinput_arming_input
    [ sens_cal_input
--    , rcinput_mode_input
    ]
    [ telem_arming_input
    , px4io_state_input
    ]
    arming_output
    arming_status

  where
  rcinput_arming_input = ArmingInput
    { ai_name = "rcinput"
    , ai_chan = lawinput_rcinput_arming
    , ai_get  = deref
    , ai_set  = A.rcinput
    }
  telem_arming_input = ConstantArmingInput $ ArmingInput
    { ai_name = "telem"
    , ai_chan = lawinput_telem_arming
    , ai_get  = deref
    , ai_set  = A.telem
    }
  px4io_state_input = ConstantArmingInput $ ArmingInput
    { ai_name = "px4io"
    , ai_chan = lawinput_px4io_state
    , ai_get  = arming_from_px4io_state
    , ai_set  = A.px4io
    }
  sens_cal_input = InitialArmingInput $ ArmingInput
    { ai_name = "sens_valid"
    , ai_chan = lawinput_sensors_output
    , ai_get  = arming_from_sensors
    , ai_set  = A.sens_valid
    }
{-
  rcinput_mode_input = InitialArmingInput $ ArmingInput
    { ai_name = "rcinput_mode"
    , ai_chan = lawinput_rcinput_modes
    , ai_get  = arming_from_mode
    , ai_set  = A.rcinput_mode
    }
-}

  arming_from_px4io_state s = do
    safety_off <- deref (s ~> PX4.status ~> PX4.safety_off)
    return (safety_off ? (T.neutral, T.negative))

  arming_from_sensors sens = do
    v <- deref (sens ~> S.valid)
    return (v ? (T.neutral, T.negative))

{-
  arming_from_mode m = do
    tm <- deref (m ~> CM.thr_mode)
    return ((tm ==? TM.directUi) ? (T.neutral, T.negative))
-}
