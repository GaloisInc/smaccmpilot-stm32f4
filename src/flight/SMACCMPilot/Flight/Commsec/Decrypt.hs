{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.Flight.Commsec.Decrypt where

import qualified SMACCMPilot.Flight.Commsec.Commsec as C
import qualified SMACCMPilot.Shared                 as S
import qualified Commsec.CommsecOpts                as O

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

--------------------------------------------------------------------------------

decryptTask :: (SingI n0, SingI n1)
            => O.Options
            -> ChannelSink   n0  S.CommsecArray -- from datalink
            -> ChannelSource n1  S.MavLinkArray -- to GCS Rx task
            -> Task p ()
decryptTask opts rx tx = do
  emitter <- withChannelEmitter tx "decToGcsRxSrc"
  onChannel rx "hxToDecRcv" $ \pkgStream -> do
    pkg <- local (iarray [])
    refCopy pkg pkgStream
    res <- C.decrypt C.uavCtx pkg
    -- Check that the tags match
    -- XXX report on bad messages?
    when (res ==? 0) $ do
      -- Copy the decrypted message out of the pkg
      payload <- local (iarray [] :: Init S.MavLinkArray)
      C.copyFromPkg pkg payload
      emit_ emitter (constRef payload)

  taskModuleDef $ depend (C.commsecModule opts)


