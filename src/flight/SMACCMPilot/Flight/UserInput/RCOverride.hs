{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Handle RC Override MAVLink messages from the GCS.

module SMACCMPilot.Flight.UserInput.RCOverride
  ( userRCOverrideTower
  ) where

import Control.Monad hiding (when)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.UserInput.Decode             as D
import qualified SMACCMPilot.Mavlink.Messages.RcChannelsOverride as O
import qualified SMACCMPilot.Flight.Types.UserInput              as T
import qualified SMACCMPilot.Flight.Types.ControlSource          as CS
import qualified SMACCMPilot.Flight.Types.ArmedMode              as A

--------------------------------------------------------------------------------

-- | This process periodically reports the latest rcOverride message from the
-- GCS as well as the time since a message was received.

userRCOverrideTower :: SingI n
                    => ChannelSink n (Struct "rc_channels_override_msg")
                    -> DataSink (Stored A.ArmedMode)
                    -> Tower p ( DataSink (Struct "userinput_result")
                               , DataSink (Stored IBool)
                               )
userRCOverrideTower override_msg_sink armed_sink = do
  active <- dataport
  ui <- dataport
  task "userMAVInput" $ userMAVInputTask armed_sink
                                         override_msg_sink
                                         (src active)
                                         (src ui)
  return (snk ui, snk active)



userMAVInputTask :: SingI n
                    -- Arming result
                 => DataSink (Stored A.ArmedMode)
                    -- From GCS Rx Task
                 -> ChannelSink n (Struct "rc_channels_override_msg")
                    -- Joystick failsafe button
                 -> DataSource (Stored IBool)
                    -- To UserInput.Mux task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userMAVInputTask a rc_over_snk src_js_fs src_rc_over_res = do
  -- Arming result
  armedReader      <- withDataReader a "armedReader"
  -- processed result
  rcOverrideWriter <- withDataWriter src_rc_over_res "rc_over_res_tx"
  failSafeWriter   <- withDataWriter src_js_fs "src_js_fs"

  armedRef         <- taskLocal "armedRef"
  ovr_msg_local    <- taskLocal "over_msg_local"
  chs              <- taskLocal "channels"
  uiResult         <- taskLocal "userinput_res"
  jsFailSafe       <- taskLocal "jsFailSafe"

  timer <- withGetTimeMillis

  -- XXX Need a channel, since we don't want to process messages unless we
  -- really got an override event.  But how do we know we're getting "fresh
  -- enough" messages?
  onChannel rc_over_snk "rc_over_snk" $ \ovr_msg ->  do
    readData armedReader armedRef
    armed <- deref armedRef
    when (armed ==? A.armed) $ do
      now <- getTimeMillis timer
      refCopy ovr_msg_local ovr_msg
      -- Translate the MAVLink message.
      call_ processOverrideMsg chs ovr_msg_local

      validInputs <- call validOverrideMsg (constRef chs)
      ifte_ validInputs
        -- Decode the PPMs.  This should ONLY be called on filtered inputs.
        (call_ D.userInputDecode chs uiResult now)
        (call_ D.userInputFailsafe uiResult now)
      store (uiResult ~> T.source) CS.mavlink
      store jsFailSafe validInputs
      -- Send it to the Mux task.
      writeData failSafeWriter (constRef jsFailSafe)
      writeData rcOverrideWriter (constRef uiResult)

  taskModuleDef $ do
    depend D.userInputDecodeModule
    depend T.userInputTypeModule
    incl processOverrideMsg
    incl validOverrideMsg

-- Copy the PPM values out of the rc_channels_override_msg struct into an array.
processOverrideMsg :: Def ('[ Ref s T.PPMs
                            , Ref s (Struct "rc_channels_override_msg")
                            ] :-> ())
processOverrideMsg = proc "processOverrideMsg" $ \arr msg -> body $ do
  let go :: (Integer, Label "rc_channels_override_msg" (Stored Uint16))
         -> Ivory eff ()
      go (ix, chan) = do
        field <- msg ~>* chan
        store (arr ! toIx (fromInteger ix :: Sint32)) (safeCast field)
  mapM_ go channelAccessors

  where
  channelAccs = [ O.chan1_raw, O.chan2_raw, O.chan3_raw, O.chan4_raw
                , O.chan5_raw, O.chan6_raw, O.chan7_raw, O.chan8_raw
                -- Ignore target fields.
                ]
  channelAccessors = zip [0..7] channelAccs

-- Filter RC override messages, checking that the fields are as expected AND the
-- kill switch is on.  Returns True if the conditions are met.
validOverrideMsg :: Def ('[ConstRef s T.PPMs] :-> IBool)
validOverrideMsg = proc "validOverrideMsg" $ \arr -> body $ do
  -- See if our failsafe button is depressed (channel 5, counting from 1).
  -- We can be exact here (2000) since it's a MAVLink msg.
  kill <- deref (arr ! 4)

  let access b ix = do
        v <- deref (arr ! toIx (fromInteger ix :: Sint32))
        return (b .&& validRange v)

  -- Fold across rpyt
  let validRanges = foldM access true [0..4]

  vs <- validRanges
  ret $ (kill ==? 2000) .&& vs

  where
  validRange i = i >=? 1000 .&& i <=? 2000




