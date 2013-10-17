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

userMAVInputTask :: SingI n
                    -- Arming result
                 => DataSink (Stored A.ArmedMode)
                    -- From GCS Rx Task
                 -> ChannelSink n (Struct "rc_channels_override_msg")
                    -- To UserInput.Mux task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userMAVInputTask a rc_over_snk src_rc_over_res = do
  -- Arming result
  armedReader      <- withDataReader a "armedReader"
  -- processed result
  rcOverrideWriter <- withDataWriter src_rc_over_res "rc_over_res_tx"

  armedRef         <- taskLocal "armedRef"
  ovr_msg_local    <- taskLocal "over_msg_local"
  chs              <- taskLocal "channels"
  uiResult         <- taskLocal "userinput_res"

  timer <- withGetTimeMillis

  -- XXX Need a channel, since we don't want to process messages unless we
  -- really got an override event.  But how do we know we're getting "fresh
  -- enough" messages?
  onChannel rc_over_snk "rc_over_snk" $ \ovr_msg ->  do
    readData armedReader armedRef
    armed <- deref armedRef
    when (armed ==? A.as_ARMED) $ do
      now <- getTimeMillis timer
      refCopy ovr_msg_local ovr_msg
      -- Translate the MAVLink message.
      call_ processOverrideMsg chs ovr_msg_local
      -- Decode the PPMs.
      call_ D.userInputDecode chs uiResult now
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
