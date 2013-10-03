{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Take airdata messages from SMACCMPilot, encrypt them, and send them to the
-- datalink task.

module SMACCMPilot.Flight.Commsec.Encrypt where

import qualified SMACCMPilot.Flight.Commsec.Commsec   as C
import qualified SMACCMPilot.Shared                   as S

import           Ivory.Tower
import           Ivory.Language
import           Ivory.Stdlib

--------------------------------------------------------------------------------

encryptTask :: (SingI n0, SingI n1)
            => ChannelSink   n0 S.MavLinkArray -- from GCS Tx
            -> ChannelSource n1 S.CommsecArray -- to datalink
            -> Task p ()
encryptTask rx tx = do
  emitter <- withChannelEmitter tx "encToHxSrc"

  -- Sets up commsec for both encryption and decryption.
  taskInit C.setupCommsec

  onChannel rx "gcsTxToEnc" $ \mavStream -> do
    mav <- local (iarray [])
    refCopy mav mavStream

    pkg <- local (iarray [] :: Init S.CommsecArray)
    C.copyToPkg (constRef mav) pkg
    C.encrypt C.uavCtx pkg
    emit_ emitter (constRef pkg)

  taskModuleDef $ depend C.commsecModule



