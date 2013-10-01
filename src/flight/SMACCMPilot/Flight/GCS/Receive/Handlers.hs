{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.GCS.Receive.Handlers where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Mavlink.Messages.RequestDataStream as RDS
import qualified SMACCMPilot.Mavlink.Receive as R
import           SMACCMPilot.Mavlink.Unpack

import           SMACCMPilot.Flight.GCS.Stream (updateGCSStreamPeriods)

paramRequestList :: Ref s (Struct "param_request_list_msg") -> Ivory eff ()
paramRequestList _ =
  -- XXX need to implement.
  return ()

paramRequestRead :: Ref s (Struct "param_request_read_msg") -> Ivory eff ()
paramRequestRead _ =
  -- XXX need to implement. requires local allocation, so could be best to wait
  return ()

paramSet :: Ref s (Struct "param_set_msg") -> Ivory eff ()
paramSet _ =
  -- XXX need to implement. requires local allocation, so could be best to wait
  return ()

requestDatastream :: Ref s1 (Struct "gcsstream_timing")
                  -> Ref s2 (Struct "request_data_stream_msg") -> Ivory eff ()
requestDatastream streamperiods msg = do
  rsid   <- deref (msg ~> RDS.req_stream_id)
  enable <- deref (msg ~> RDS.start_stop)
  rate   <- deref (msg ~> RDS.req_message_rate)
  updateGCSStreamPeriods streamperiods rsid (enable >? 0) rate

hilState :: (SingI n, GetAlloc eff ~ Scope cs)
         => ChannelEmitter n (Struct "hil_state_msg")
         -> Ref s (Struct "hil_state_msg")
         -> Ivory eff ()
hilState e r = emit_ e (constRef r)

-- | Handles a specific Mavlink message, where 'unpack' is a method of the
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

handlerModuleDefs :: ModuleDef
handlerModuleDefs = mapM_ depend mavlinkMessageModules

