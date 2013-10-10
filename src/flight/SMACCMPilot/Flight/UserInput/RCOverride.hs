{-# LANGUAGE DataKinds #-}

-- | Handle RC Override MAVLink messages from the GCS.

module SMACCMPilot.Flight.UserInput.RCOverride
  ( userMAVInputTask
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.UserInput.Decode as D

--------------------------------------------------------------------------------

userMAVInputTask :: -- From GCS Rx Task
                    DataSink (Struct "rc_channels_override_msg")
                    -- To UserInput.Mux task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userMAVInputTask snk_rc_over_msg src_rc_over_res = do
  -- processed result
  rcOverRideWriter <- withDataWriter src_rc_over_res "rc_over_res_tx"
  -- Mavlink result
  rcOverRideReader <- withDataReader snk_rc_over_msg "rc_over_msg_rx"

  -- Value of the last MAVLink messsage received.
  lastMAV_res      <- taskLocal "rc_overide_mav"
  -- PPM signals
  chs              <- taskLocal "channels"
  -- Decoder state
  decoder          <- taskLocal "decoder"
  -- input_result we'll write to MUX
  uiResult         <- taskLocal "userinput_res"

  -- Process next override msg at the same rate at the PPM receiver.  XXX
  -- MAVLink doesn't run this fast, so most of the time we'll see no change to
  -- the override messages.
  onPeriod 50 $ \now -> do
    call_ userInputDecode chs uiResult now 
    writeData XXX

  taskModuleDef $ do
    userInputDecodeModule
