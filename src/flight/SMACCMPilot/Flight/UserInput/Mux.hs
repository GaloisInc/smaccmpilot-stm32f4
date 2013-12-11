{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Takes input from PPM RC controller and from the RC Overide task and
-- multiplexes them for output to the motor control.

module SMACCMPilot.Flight.UserInput.Mux
  ( userInputMuxTower
  , armedMuxTask
  , controlLawMuxTower
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.ArmedMode  as A
import qualified SMACCMPilot.Flight.Types.UserInput  as T
import qualified SMACCMPilot.Flight.Types.ControlLaw as CL
import qualified SMACCMPilot.Flight.Types.ControlRequest as CR
import qualified SMACCMPilot.Flight.UserInput.Decode as D

--------------------------------------------------------------------------------

armedMuxTask :: SingI n
             => DataSink T.PPMs                    -- PPM signals
             -> ChannelSink n (Struct "control_request") -- MAVLink arming input
             -> DataSource (Stored A.ArmedMode)    -- Mux'ed arming output
             -> Task p ()
armedMuxTask ppm_input_snk ctl_req_snk armed_res_src = do
  ppmReader <- withDataReader ppm_input_snk "ppm_input_snk"
  muxWriter <- withDataWriter armed_res_src "armed_res_src"

  ppmSignals     <- taskLocal "ppmSignals"

  -- decoder arming state (from Decoder)
  armingState    <- taskLocal "arming_state"
  -- Final arming result
  armedResLocal  <- taskLocal "armedResLocal"

  let writeArmedRes :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
      writeArmedRes = writeData muxWriter (constRef armedResLocal)

  -- When we get a new event from MAVLink Rx, we check that channel 5 is armed
  -- and then set the value given from the GCS.

  -- XXX! We are assuming we get a "late enough" event here.
  -- XXX bad implementation in general
  onChannel ctl_req_snk "ctl_req_snk" $ \ctl_req -> do
    armed <- assign A.disarmed -- deref (mav_cl ~> CL.armed_mode)
    readData ppmReader ppmSignals
    switch <- call D.deadManSwitch (constRef ppmSignals)

    -- If the switch is on, store the new armed status.  If the
    -- switch is off, we'll catch it in the periodic task and
    -- disarm the motors.
    when switch $ do
      store armedResLocal armed
      writeArmedRes

  -- Also, read the latest PPM signals and see if we're arming/disarming from
  -- the PPM controller.
  onPeriod 50 $ \now -> do
    readData ppmReader ppmSignals
    changed <- call D.armingStatemachine ppmSignals armingState armedResLocal now
    switch  <- call D.deadManSwitch (constRef ppmSignals)

    -- If the switch is on and the armed status has changed, set
    -- it as the new arming status.  Otherwise, if the switch is
    -- off, disarm the motors regardless of the arm state.
    ifte_ switch
      (when changed writeArmedRes)
      (store armedResLocal A.safe >> writeArmedRes)

  taskModuleDef $ do
    depend D.userInputDecodeModule

--------------------------------------------------------------------------------

controlLawMuxTower :: DataSink T.PPMs
                   -> DataSink (Stored A.ArmedMode)
                   -> Tower p (DataSink (Struct "control_law"))
controlLawMuxTower ppm_chans armed_state = do
  law <- dataport
  task "controlLawMuxTask" $ controlLawMuxTask ppm_chans
                                               armed_state
                                               (src law)
  return (snk law)



controlLawMuxTask :: DataSink T.PPMs                    -- PPM signals
                  -> DataSink (Stored A.ArmedMode)      -- Mux'ed arming input
                  -> DataSource (Struct "control_law")  -- flightmode output
                  -> Task p ()
controlLawMuxTask ppm_input_snk mav_armed_snk law_src = do
  ppmReader   <- withDataReader ppm_input_snk "ppm_input_snk"
  armedReader <- withDataReader mav_armed_snk "mav_armed_snk"
  lawWriter   <- withDataWriter law_src "law_src"
  ppmSignals  <- taskLocal "ppmSignals"
  modeState   <- taskLocal "flightmode_state"

  onPeriod 50 $ \now -> do
    armed_ref  <- local izero
    readData armedReader armed_ref
    armed      <- deref armed_ref
    law        <- local (istruct [ CL.armed_mode .= ival armed ])

    when (armed ==? A.armed) $ do
      readData ppmReader ppmSignals
      changed <- call D.modeStatemachine ppmSignals modeState law now
      when changed $ writeData lawWriter (constRef law)

  taskModuleDef $ do
    depend D.userInputDecodeModule

--------------------------------------------------------------------------------

userInputMuxTower :: DataSink (Struct "userinput_result")
                  -> DataSink (Struct "userinput_result")
                  -> DataSink (Stored IBool)
                  -> DataSink (Struct "control_law")
                  -> Tower p (DataSink (Struct "userinput_result"))
userInputMuxTower ppm_ui override_ui override_active ctllaw = do
  result <- dataport
  task "userInputMux" $ userInputMuxTask ppm_ui
                                         override_ui
                                         override_active
                                         ctllaw
                                         (src result)
  return (snk result)

userInputMuxTask :: --- From PPM task
                    DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Stored IBool)
                    -- From flightmode mux
                 -> DataSink (Struct "control_law")
                    -- To motor control task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userInputMuxTask snk_rc_ppm snk_mav_ppm snk_mav_failsafe snk_law src_res = do
  ppmReader   <- withDataReader snk_rc_ppm  "snk_rc_ppm"
  mavReader   <- withDataReader snk_mav_ppm "snk_mav_ppm"
  lawReader   <- withDataReader snk_law     "snk_law"
  mavFSReader <- withDataReader snk_mav_failsafe "snk_mav_failsafe"
  resWriter   <- withDataWriter src_res     "src_res"

  rcLocal     <- taskLocal "rcLocal"
  lawLocal    <- taskLocal "lawLocal"
  mavLocal    <- taskLocal "mavLocal"
  mavFS       <- taskLocal "mavFSLocal"

  let writeOutput :: (GetAlloc eff ~ Scope s0)
                  => Ref s1 (Struct "userinput_result")
                  -> Ivory eff ()
      writeOutput = writeData resWriter . constRef

  onPeriod 50 $ \now -> do
    readData ppmReader rcLocal
    readData mavReader mavLocal
    readData lawReader lawLocal
    readData mavFSReader mavFS
    -- Joystick failsafe
    jsDeadMan <- deref mavFS

    armed <- deref (lawLocal ~> CL.armed_mode)
    lastMavTime <- deref (mavLocal ~> T.time)
    -- Time is monotomic.
    assert (now >=? lastMavTime)

    let mavOverrideCond =
              jsDeadMan -- Joystick deadman button
          -- depressed System armed (This shouldn't be required, as no RC
          -- override messages should be generated if the sytem isn't armed.)
          .&& (armed ==? A.armed)
          -- flight mode is auto
          -- XXX FIXME
          -- .&& (fm ==? flightModeAuto)
          -- -- Got a message recently
          .&& (now <? (lastMavTime + mavTimeout))

    ifte_ mavOverrideCond
      (writeOutput mavLocal)
      (writeOutput rcLocal)

  taskModuleDef $ depend T.userInputTypeModule

  where
  -- Timeout to revert back to the RC PPM controller for override messages.
  mavTimeout :: Uint32
  mavTimeout = 250

--------------------------------------------------------------------------------

