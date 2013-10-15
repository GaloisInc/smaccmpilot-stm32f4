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
import qualified SMACCMPilot.Flight.Types.FlightMode as FM
import qualified SMACCMPilot.Flight.UserInput.Decode as D

--------------------------------------------------------------------------------

-- Timeout to revert back to the RC PPM controller for override messages.
mavTimeout :: Uint32
mavTimeout = 500

--------------------------------------------------------------------------------

armingMuxSM :: DataReader (Array 8 (Stored Uint16))
            -> Ref s3 (Array 8 (Stored Uint16))
            -> DataWriter (Stored A.ArmedMode)
            -> Def ('[ Ref s0 (Stored IBool)
                     , Ref s1 (Stored IBool)
                     , A.ArmedMode
                     , Ref s2 (Stored A.ArmedMode)
                     ] :-> ())
armingMuxSM ppmReader ppmSignals muxWriter =
  proc "armingMuxSM"
  $ \myArmedRef youArmedRef armed armedResLocal -> body
  $ do
  -- Get the latest PPM signals
  readData ppmReader ppmSignals
  -- Get the value of the deadman's switch.
  me  <- deref myArmedRef
  you <- deref youArmedRef
  sw1 <- call D.deadManSwitch ppmSignals
  cond_ [   -- If the deadman's switch is false, kill the arming.
            iNot sw1
        ==> store armedResLocal A.as_DISARMED
            -- My msg set the arming state, but now it says to turn off.
        ,   me .&& (armed ==? A.as_DISARMED)
        ==> do store myArmedRef false
               store armedResLocal A.as_DISARMED
            -- You haven't armed the system, and I want to.
        ,   iNot you .&& (armed ==? A.as_ARMED)
        ==> do store myArmedRef true
               store armedResLocal A.as_ARMED
        ,   true
        ==> return ()
        ]
  -- Write the (possibly new) arming result.
  writeData muxWriter (constRef armedResLocal)

--------------------------------------------------------------------------------

armedMuxTask :: SingI n
             => DataSink (Array 8 (Stored Uint16)) -- PPM signals
             -> ChannelSink n (Stored A.ArmedMode) -- MAVLink arming input
             -> DataSource (Stored A.ArmedMode)    -- Mux'ed arming output
             -> Task p ()
armedMuxTask ppm_input_snk mav_armed_snk armed_res_src = do
  ppmReader <- withDataReader ppm_input_snk "ppm_input_snk"
--  mavReader <- withChannelReceiver mav_armed_snk "mav_armed_snk"
  muxWriter <- withDataWriter armed_res_src "armed_res_src"

  ppmSignals    <- taskLocal "ppmSignals"

  -- Booleans: who set the armed state (if it's set)?
  ppmSetArmed    <- taskLocalInit "ppmSetArmed" (ival false)
  mavSetArmed    <- taskLocalInit "ppmSetArmed" (ival false)
  -- Final arming result
  armedResLocal  <- taskLocal "armedResLocal"

  let armingMuxSM' = armingMuxSM ppmReader ppmSignals muxWriter

  -- When we get a new event from MAVLink Rx, we check that channel 5 is armed
  -- and then set the value given from the GCS.

  -- XXX! We are assuming we get a "late enough" event here.
  onChannelV mav_armed_snk "mav_arming" $ \armed -> do
    call_ armingMuxSM' mavSetArmed ppmSetArmed armed armedResLocal

  -- Also, read the latest PPM signals and see if we're arming/disarming from
  -- the PPM controller.
  onPeriod 50 $ \now -> -- XXX
    call_ armingMuxSM' ppmSetArmed mavSetArmed undefined armedResLocal

--------------------------------------------------------------------------------

userInputMuxTask :: -- From PPM task
                    DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Struct "userinput_result")
                    -- To motor control task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userInputMuxTask snk_rc_ppm snk_mav_ppm src_res = do
  ppmReader <- withDataReader snk_rc_ppm  "snk_rc_ppm"
  mavReader <- withDataReader snk_mav_ppm "snk_mav_ppm"
  resWriter <- withDataWriter src_res     "src_res"

  rcLocal   <- taskLocal "rcLocal"
  mavLocal  <- taskLocal "mavLocal"

  let writeOutput :: (GetAlloc eff ~ Scope s0)
                  => Ref s1 (Struct "userinput_result")
                  -> Ivory eff ()
      writeOutput = writeData resWriter . constRef

  onPeriod 50 $ \now -> do
    readData ppmReader rcLocal
    readData mavReader mavLocal

    lastMavTime <- deref (mavLocal ~> T.time)
    -- Time is monotomic.
    assert (now >=? lastMavTime)

    -- XXX
    -- isArmed <- deref (mavLocal ~> FM.armed)

    cond_
      [   -- Not armed: don't listen to MAVLink messages.
          true -- iNot isArmed XXX
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

--------------------------------------------------------------------------------

