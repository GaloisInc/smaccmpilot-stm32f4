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

decryptTask :: (SingI n0, SingI n1)
            => C.Options
            -> ChannelSink   n0  Comm.CommsecArray -- from datalink
            -> ChannelSource n1  Comm.MAVLinkArray -- to GCS Rx task
            -> DataSource        (Struct "veh_commsec_msg") -- to GCS Tx task
            -> Task p ()
decryptTask opts rx tx commsec_info_src = do
  emitter    <- withChannelEmitter tx "decToGcsRxSrc"
  commWriter <- withDataWriter commsec_info_src "commsec_info_src"

  -- For the reporter.
  reporterStruct <- taskLocalInit "commsec_reporter_struct" (istruct [])
  theTime        <- taskLocal "theTime"
  t              <- withGetTimeMillis
  taskInit $ do
    initTime <- getTimeMillis t
    store theTime initTime

  onChannel rx "hxToDecRcv" $ \pkgStream -> do

    pkg <- local (iarray [])
    refCopy pkg pkgStream
    res <- CS.decrypt CS.uavCtx pkg
    -- Check that the tags match
    -- XXX report on bad messages?
    when (res ==? 0) $ do
      -- Copy the decrypted message out of the pkg
      payload <- local (iarray [] :: Init Comm.MAVLinkArray)
      CS.copyFromPkg pkg payload
      emit_ emitter (constRef payload)

    -- Pack the reporter struct.
    reporter reporterStruct res (constRef theTime)
    writeData commWriter (constRef reporterStruct)

  taskModuleDef $ do
    depend (CS.commsecModule opts)
    depend V.vehCommsecModule

-- Gathers statistics on decryption.
reporter :: Ref s (Struct "veh_commsec_msg")
         -> Uint32
         -> ConstRef s (Stored Uint32)
         -> Ivory eff ()
reporter commRef res theTimeRef = do
  assert (res <=? 6) -- Largest error code
  store (commRef ~> V.commsec_err) (castDefault res)
  cond_ [   res ==? Comm.commsecSuccess
        ==> (commRef ~> V.good_msgs)   += 1
        ,   true
        ==> do (commRef ~> V.bad_msgs) += 1
               t <- deref theTimeRef
               (commRef ~> V.time) += t
        ]

--do goods <- deref store (ref ~> V.good_msgs)

--   curr <- getTimeMillis t
--   return ()



