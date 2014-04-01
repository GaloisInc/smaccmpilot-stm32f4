{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.Flight.Commsec.Decrypt where

import qualified SMACCMPilot.Flight.Commsec.Commsec      as CS
import qualified SMACCMPilot.Communications              as Comm
import qualified SMACCMPilot.Flight.Commsec.CommsecOpts  as C
import qualified SMACCMPilot.Mavlink.Messages.VehCommsec as V

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower hiding (src)

--------------------------------------------------------------------------------

decryptTask :: C.Options
            -> ChannelSink   Comm.CommsecArray -- from datalink
            -> ChannelSource Comm.MAVLinkArray -- to GCS Rx task, recovery task
            -> ChannelSource (Struct "veh_commsec_msg") -- to GCS Tx task
            -> Task p ()
decryptTask opts rx tx commsec_info_src = do
  pt_emitter <- withChannelEmitter tx "plaintext"
  info_emitter <- withChannelEmitter commsec_info_src "commsec_info"

  -- For the reporter.
  reporterStruct <- taskLocalInit "commsec_reporter_struct" (istruct [])
  lastGoodTime   <- taskLocalInit "lastGoodTime" (ival 0)

  ct_evt <- withChannelEvent rx "ct_chan"
  handle ct_evt "ct_evt" $ \pkgStream -> do
    pkg <- local (iarray [])
    refCopy pkg pkgStream
    res <- CS.decrypt CS.uavCtx pkg
    -- Check that the tags match
    when (res ==? 0) $ do
      -- Copy the decrypted message out of the pkg
      payload <- local (iarray [] :: Init Comm.MAVLinkArray)
      CS.copyFromPkg pkg payload
      emit_ pt_emitter (constRef payload)

    -- Pack the reporter struct.
    now <- getTime
    reporter reporterStruct res now lastGoodTime
    emit_ info_emitter (constRef reporterStruct)

  taskModuleDef $ do
    depend (CS.commsecModule opts)
    depend V.vehCommsecModule

-- Gathers statistics on decryption.
reporter :: Ref      s (Struct "veh_commsec_msg")
         -> Uint32
         -> ITime 
         -> Ref      s (Stored ITime)
         -> Ivory eff ()
reporter commRef res now lastGoodTime = do
  assert (res <=? 6) -- Largest error code
  store (commRef ~> V.commsec_err) (castDefault res)
  cond_
    [ res ==? Comm.commsecSuccess ==> do
       (commRef ~> V.good_msgs)   += 1
       store lastGoodTime now
       store (commRef ~> V.time) 0
    , true ==> do
       (commRef ~> V.bad_msgs) += 1
       l <- deref lastGoodTime
       dt_ms <- assign (castWith 0 (toIMilliseconds (now - l)))
       store (commRef ~> V.time) dt_ms
    ]


