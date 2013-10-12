{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.GCS.Receive.Handlers where

import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Mavlink.Messages.RequestDataStream  as RDS
import qualified SMACCMPilot.Mavlink.Messages.ParamSet           as PS
import qualified SMACCMPilot.Mavlink.Messages.ParamRequestRead   as PRR

import qualified SMACCMPilot.Mavlink.Messages.RcChannelsOverride as O
import qualified SMACCMPilot.Flight.Types.UserInput              as T

import qualified SMACCMPilot.Mavlink.Receive                     as R
import           SMACCMPilot.Mavlink.Unpack

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.GCS.Stream (updateGCSStreamPeriods)

--------------------------------------------------------------------------------
-- Params

-- | Send a request to the GCS transmit task to send each parameter.
paramRequestList :: (SingI n)
                 => [Param ParamReader]
                 -> ChannelEmitter n (Stored Sint16)
                 -> Ref s (Struct "param_request_list_msg")
                 -> Ivory (ProcEffects cs r) ()
paramRequestList params emitter _ =
  forM_ [0..length params] $ \n -> do
    emitV_ emitter (fromIntegral n)

-- | Request read of a parameter by name or index.
paramRequestRead :: (SingI n)
                 => ParamIndexGetter
                 -> ChannelEmitter n (Stored Sint16)
                 -> Ref s (Struct "param_request_read_msg")
                 -> Ivory (ProcEffects cs r) ()
paramRequestRead getIndex emitter msg = do
  n     <- deref (msg ~> PRR.param_index)
  ixRef <- local (ival n)

  -- Look up by name if index is -1.
  when (n ==? -1) $ do
    name <- local (istr_empty 16)
    istr_from_sz name (constRef $ msg ~> PRR.param_id)
    call_ (paramIndexGetter getIndex) (constRef name) ixRef
    -- ixRef stays -1 if the name lookup fails

  ix <- deref ixRef
  unless (ix ==? -1) $ do
    emitV_ emitter ix

-- | Set a parameter's value and send the modified param info.
paramSet :: (SingI n)
         => ParamIndexGetter
         -> Def ('[Sint16, IFloat] :-> IBool)
         -> ChannelEmitter n (Stored Sint16)
         -> Ref s3 (Struct "param_set_msg")
         -> Ivory (ProcEffects cs r) ()
paramSet getIndex setValue emitter msg = do
  name  <- local (istr_empty 16)
  istr_from_sz name (constRef $ msg ~> PS.param_id)

  ixRef <- local (ival (0 :: Sint16))
  found <- call (paramIndexGetter getIndex) (constRef name) ixRef

  when found $ do
    ix  <- deref ixRef
    val <- deref (msg ~> PS.param_value)
    call_  setValue ix val
    emitV_ emitter ix

--------------------------------------------------------------------------------

requestDatastream :: Ref s1 (Struct "gcsstream_timing")
                  -> (ConstRef s1 (Struct "gcsstream_timing") -> Ivory eff ())
                  -> Ref s2 (Struct "request_data_stream_msg")
                  -> Ivory eff ()
requestDatastream streamperiods send msg = do
  rsid   <- deref (msg ~> RDS.req_stream_id)
  enable <- deref (msg ~> RDS.start_stop)
  rate   <- deref (msg ~> RDS.req_message_rate)
  updateGCSStreamPeriods streamperiods rsid (enable >? 0) rate
  send (constRef streamperiods)

--------------------------------------------------------------------------------

hilState :: (SingI n, GetAlloc eff ~ Scope cs)
         => ChannelEmitter n (Struct "hil_state_msg")
         -> Ref s (Struct "hil_state_msg")
         -> Ivory eff ()
hilState e r = emit_ e (constRef r)

--------------------------------------------------------------------------------

-- | Handle RC override messages.  Copy the PPM fields of the
-- rc_channels_override_msg into our time-stamped struct.  This is ugly since we
-- can't easily copy array fields.
rcOverride :: (GetAlloc eff ~ Scope cs)
           => DataWriter (Struct "timestamped_rc_override")
           -> OSGetTimeMillis
           -> Ref s (Struct "rc_channels_override_msg")
           -> Ivory eff ()
rcOverride dtx timeGetter msgRef = do
  t <- getTimeMillis timeGetter
  res <- local (istruct [T.rc_msg .= istruct [], T.rc_time .= ival t])
  let msg = res ~> T.rc_msg

  let accs = [ O.chan1_raw, O.chan2_raw, O.chan3_raw, O.chan4_raw
             , O.chan5_raw, O.chan6_raw, O.chan7_raw, O.chan8_raw
             -- Ignore target fields.
             ]
  let go :: Label "rc_channels_override_msg" (Stored Uint16)
         -> Ivory eff ()
      go chan = do
        field <- deref (msgRef ~> chan)
        store (msg ~> chan) field
  mapM_ go accs

  writeData dtx (constRef res)

--------------------------------------------------------------------------------

-- | Handles a specific Mavlink message, where 'unpackMsg' is a method of the
-- 'MavlinkUnpackageMsg'.
handle :: (GetAlloc eff ~ Scope s, MavlinkUnpackableMsg t, IvoryStruct t)
       => (Ref (Stack s) (Struct t) -> Ivory eff ())
       -> Ref s1 (Struct "mavlink_receive_state")
       -> Ivory eff ()
handle handler rxstate = do
  let (unpacker, msgid) = unpackMsg
  rxid <- deref (rxstate ~> R.msgid)
  msg  <- local (istruct [])
  when (rxid ==? msgid) $ do
    call_ unpacker msg (toCArray (constRef (rxstate ~> R.payload)))
    handler msg

--------------------------------------------------------------------------------

handlerModuleDefs :: ModuleDef
handlerModuleDefs = mapM_ depend mavlinkMessageModules

--------------------------------------------------------------------------------

