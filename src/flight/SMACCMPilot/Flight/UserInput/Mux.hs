{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Takes input from PPM RC controller and from the RC Overide task and
-- multiplexes them for output to the motor control.

module SMACCMPilot.Flight.UserInput.Mux
  ( userInputMuxTask
  , armedMuxTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.Armed      as A
import qualified SMACCMPilot.Flight.Types.UserInput  as T
import qualified SMACCMPilot.Flight.UserInput.Decode as D

--------------------------------------------------------------------------------

armedMuxTask :: SingI n
             => DataSink (Array 8 (Stored Uint16)) -- PPM signals
             -> ChannelSink n (Stored A.ArmedMode) -- MAVLink arming input
             -> DataSource (Stored A.ArmedMode)    -- Mux'ed arming output
             -> Task p ()
armedMuxTask ppm_input_snk mav_armed_snk armed_res_src = do
  ppmReader <- withDataReader ppm_input_snk "ppm_input_snk"
  muxWriter <- withDataWriter armed_res_src "armed_res_src"

  ppmSignals     <- taskLocal "ppmSignals"

  -- decoder arming state (from Decoder)
  armingState    <- taskLocal "arming_state"
  -- Final arming result
  armedResLocal  <- taskLocal "armedResLocal"

  -- When we get a new event from MAVLink Rx, we check that channel 5 is armed
  -- and then set the value given from the GCS.

  -- XXX! We are assuming we get a "late enough" event here.
  onChannelV mav_armed_snk "mav_arming" $ \armed -> do
    readData ppmReader ppmSignals
    switch <- call D.deadManSwitch (constRef ppmSignals)

    -- If the switch is on, store the new armed status.  If the
    -- switch is off, we'll catch it in the periodic task and
    -- disarm the motors.
    when switch $ do
      store armedResLocal armed
      writeData muxWriter (constRef armedResLocal)

  -- Also, read the latest PPM signals and see if we're arming/disarming from
  -- the PPM controller.
  onPeriod 50 $ \now -> do
    changed <- call D.armingStatemachine ppmSignals armingState armedResLocal now

    readData ppmReader ppmSignals
    switch <- call D.deadManSwitch (constRef ppmSignals)

    -- If the switch is on and the armed status has changed, set
    -- it as the new arming status.  Otherwise, if the switch is
    -- off, disarm the motors regardless of the arm state.
    ifte_ switch
      (when changed $
         writeData muxWriter (constRef armedResLocal))
      (do store armedResLocal A.as_DISARMED
          writeData muxWriter (constRef armedResLocal))

  taskModuleDef $ do
    depend D.userInputDecodeModule

--------------------------------------------------------------------------------

userInputMuxTask :: -- From Arming Mux task
                    DataSink (Stored A.ArmedMode)
                    -- From PPM task
                 -> DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Struct "userinput_result")
                    -- To motor control task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userInputMuxTask snk_armed snk_rc_ppm snk_mav_ppm src_res = do
  armReader <- withDataReader snk_armed   "snk_armed"
  ppmReader <- withDataReader snk_rc_ppm  "snk_rc_ppm"
  mavReader <- withDataReader snk_mav_ppm "snk_mav_ppm"
  resWriter <- withDataWriter src_res     "src_res"

  armLocal  <- taskLocal "armLocal"
  rcLocal   <- taskLocal "rcLocal"
  mavLocal  <- taskLocal "mavLocal"

  let writeOutput :: (GetAlloc eff ~ Scope s0)
                  => Ref s1 (Struct "userinput_result")
                  -> Ivory eff ()
      writeOutput = writeData resWriter . constRef

  onPeriod 50 $ \now -> do
    readData armReader armLocal
    armed <- deref armLocal
    readData ppmReader rcLocal
    readData mavReader mavLocal

    lastMavTime <- deref (mavLocal ~> T.time)
    -- Time is monotomic.
    assert (now >=? lastMavTime)

    cond_
      [   -- Not armed: don't listen to MAVLink messages.  This shouldn't be
          -- required, as no RC override messages should be generated if the
          -- sytem isn't armed.
          (armed /=? A.as_ARMED)
      ==> writeOutput rcLocal
          -- No MAVLink message has arrived in the past mavTime milliseconds.
          -- Ignore MAV input.
      ,   (now >=? (lastMavTime + mavTimeout))
      ==> writeOutput rcLocal
          -- Otherwise, we can use the MAVLink RC override.
      ,   true
      ==> writeOutput mavLocal
      ]

  taskModuleDef $
    depend T.userInputTypeModule

  where
  -- Timeout to revert back to the RC PPM controller for override messages.
  mavTimeout :: Uint32
  mavTimeout = 500

--------------------------------------------------------------------------------

