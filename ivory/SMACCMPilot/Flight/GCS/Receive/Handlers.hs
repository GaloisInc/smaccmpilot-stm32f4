{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.GCS.Receive.Handlers where

import Ivory.Language
import Ivory.Stdlib

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Mavlink.Messages.RequestDataStream as RDS
import qualified SMACCMPilot.Mavlink.Receive as R
import           SMACCMPilot.Mavlink.Unpack

import           SMACCMPilot.Flight.GCS.Stream (updateGCSStreamPeriods)

import qualified SMACCMPilot.Param as P

paramRequestList :: Ref s (Struct "param_request_list_msg") -> Ivory eff ()
paramRequestList _ = do
  infotbl <- addrOf P.param_info
  count <- (deref =<< addrOf P.param_count)
  arrayMap $ \ix -> do
    when (ix <? count) $ do
      store ((infotbl ! ix) ~> P.param_requested) 1

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

hilState :: Ref s (Struct "hil_state_msg") -> Ivory eff ()
hilState _ =
  -- XXX need to implement.
  return ()

handle :: (eff `AllocsIn` s, MavlinkUnpackableMsg t, IvoryStruct t)
       => (Ref (Stack s) (Struct t) -> Ivory eff () )
       -> Ref s1 (Struct "mavlink_receive_state")
       -> Ivory eff ()
handle handler rxstate = do
  let (unpacker, msgid) = unpackMsg
  rxid <- deref (rxstate ~> R.msgid)
  msg  <- local (istruct [])
  ifte_ (rxid /=? msgid) (return ()) $ do
    call_ unpacker msg (toCArray (constRef (rxstate ~> R.payload)))
    handler msg

handlerModuleDefs :: ModuleDef
handlerModuleDefs = do
  mapM_ depend mavlinkMessageModules
  depend P.paramModule

