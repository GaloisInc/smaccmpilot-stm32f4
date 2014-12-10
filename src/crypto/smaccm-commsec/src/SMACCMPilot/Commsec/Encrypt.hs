{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Take airdata messages from SMACCMPilot, encrypt them, and send them to the
-- datalink task.

module SMACCMPilot.Flight.Commsec.Encrypt where

import qualified SMACCMPilot.Flight.Commsec.Commsec     as CS
import qualified SMACCMPilot.Flight.Commsec.CommsecOpts as C
import qualified SMACCMPilot.Communications             as Comm

import           Ivory.Tower
import           Ivory.Language

--------------------------------------------------------------------------------

encryptTask :: C.Options
            -> ChannelSink   Comm.MAVLinkArray -- from GCS Tx
            -> ChannelSource Comm.CommsecArray -- to datalink
            -> Task p ()
encryptTask opts rx tx = do
  emitter <- withChannelEmitter tx "encToHxSrc"

  -- Sets up commsec for both encryption and decryption.
  taskInit (CS.setupCommsec opts)
  pt_evt <- withChannelEvent rx "plaintext_evt"
  handle pt_evt "pt_handler" $ \mavStream -> do
    pkg <- local (iarray [] :: Init Comm.CommsecArray)
    CS.copyToPkg mavStream pkg
    CS.encrypt CS.uavCtx pkg
    emit_ emitter (constRef pkg)

  taskModuleDef $ depend (CS.commsecModule opts)

