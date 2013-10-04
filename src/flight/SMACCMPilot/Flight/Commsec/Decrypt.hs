{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.Flight.Commsec.Decrypt where

import qualified SMACCMPilot.Flight.Commsec.Commsec as CS
import qualified SMACCMPilot.Communications         as C

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

--------------------------------------------------------------------------------

decryptTask :: (SingI n0, SingI n1)
            => ChannelSink   n0  C.CommsecArray -- from datalink
            -> ChannelSource n1  C.MAVLinkArray -- to GCS Rx task
            -> Task p ()
decryptTask rx tx = do
  emitter <- withChannelEmitter tx "decToGcsRxSrc"
  onChannel rx "hxToDecRcv" $ \pkgStream -> do
    pkg <- local (iarray [])
    refCopy pkg pkgStream
    res <- CS.decrypt CS.uavCtx pkg
    -- Check that the tags match
    -- XXX report on bad messages?
    when (res ==? 0) $ do
      -- Copy the decrypted message out of the pkg
      payload <- local (iarray [] :: Init C.MAVLinkArray)
      CS.copyFromPkg pkg payload
      emit_ emitter (constRef payload)

  taskModuleDef $ depend CS.commsecModule


