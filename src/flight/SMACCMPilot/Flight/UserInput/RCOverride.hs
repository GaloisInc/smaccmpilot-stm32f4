{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Handle RC Override MAVLink messages from the GCS.

module SMACCMPilot.Flight.UserInput.RCOverride
  ( userMAVInputTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.UserInput.Decode             as D
import qualified SMACCMPilot.Mavlink.Messages.RcChannelsOverride as O
import qualified SMACCMPilot.Flight.Types.UserInput              as T
import qualified SMACCMPilot.Flight.Types.Armed                  as A

--------------------------------------------------------------------------------

-- | This process periodically reports the latest rcOverride message from the
-- GCS as well as the time since a message was received.
userMAVInputTask :: DataSink (Stored A.ArmedMode)
                    -- From GCS Rx Task
                 -> DataSink (Struct "timestamped_rc_override")
                    -- To UserInput.Mux task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userMAVInputTask a snk_rc_over_msg src_rc_over_res = do
  -- Arming result
  armedReader      <- withDataReader a "armedReader"
  -- Mavlink result
  rcOverrideReader <- withDataReader snk_rc_over_msg "rc_over_msg_rx"
  -- processed result
  rcOverrideWriter <- withDataWriter src_rc_over_res "rc_over_res_tx"

  armedRef         <- taskLocal "armedRef"
  -- Value of the last MAVLink messsage received.
  currMAV_res      <- taskLocal "rc_overide_mav_curr"
  -- PPM signals
  chs              <- taskLocal "channels"
  -- input_result we'll write to MUX
  uiResult         <- taskLocal "userinput_res"

  -- Process next override msg at the same rate at the PPM receiver.  XXX
  -- MAVLink doesn't run this fast, so most of the time we'll see no change to
  -- the override messages.
  onPeriod 50 $ \_now -> do
    readData armedReader armedRef
    armed <- deref armedRef
    when (armed ==? A.as_ARMED) $ do
      readData rcOverrideReader currMAV_res
      -- Get the timestamp for the message.
      t <- deref (currMAV_res ~> T.rc_time)
      -- Translate the MAVLink message.
      call_ processOverrideMsg chs (currMAV_res ~> T.rc_msg)
      -- Decode the PPMs using the timestamp of the message, not the current time.
      -- We want to know when the last sent message was.
      call_ D.userInputDecode chs uiResult t
      -- Send it to the Mux task.
      writeData rcOverrideWriter (constRef uiResult)

  taskModuleDef $ do
    depend D.userInputDecodeModule
    depend T.userInputTypeModule
    incl processOverrideMsg

-- Copy the PPM values out of the rc_channels_override_msg struct into an array.
processOverrideMsg :: Def ('[ Ref s (Array 8 (Stored Uint16))
                            , Ref s (Struct "rc_channels_override_msg")
                            ] :-> ())
processOverrideMsg = proc "processOverrideMsg" $ \arr msg -> body $ do
  let accs = [ O.chan1_raw, O.chan2_raw, O.chan3_raw, O.chan4_raw
             , O.chan5_raw, O.chan6_raw, O.chan7_raw, O.chan8_raw
             -- Ignore target fields.
             ]
  let go :: (Integer, Label "rc_channels_override_msg" (Stored Uint16))
         -> Ivory eff ()
      go (ix, chan) = do
        field <- deref (msg ~> chan)
        store (arr ! toIx (fromInteger ix :: Sint32)) field

  mapM_ go (zip [0..7] accs)
