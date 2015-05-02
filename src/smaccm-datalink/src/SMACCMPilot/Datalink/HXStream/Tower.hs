{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Datalink.HXStream.Tower
  ( hxstreamEncodeTower
  , hxstreamDecodeTower
  , HXCyphertext
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import qualified SMACCMPilot.Datalink.HXStream.Ivory as H
import SMACCMPilot.Commsec.Sizes

airDataTag :: H.Tag
airDataTag = 0

[ivory| string struct HXCyphertext 195 |] -- 96*2+3==195

hxstreamEncodeTower :: String
                    -> ChanOutput CyphertextArray
                    -> BackpressureTransmit HXCyphertext (Stored IBool)
                    -> Tower e ()
hxstreamEncodeTower n ct_chan (BackpressureTransmit serial_chan complete) = do
  let bufmod = package "hx_cyphertext" $ defStringType (Proxy :: Proxy HXCyphertext)
  towerModule bufmod
  towerDepends bufmod
  towerModule $ H.hxstreamModule
  monitor (n ++ "_datalink_encode") $ do
    monitorModuleDef $ depend H.hxstreamModule
    pending <- state "pending"

    handler ct_chan "encoder_ct_in" $ do
      e <- emitter serial_chan 1
      callback $ \ct -> do
        already_pending <- deref pending
        unless already_pending $ do
          buf <- local izero
          H.encodeString airDataTag ct buf
          emit e $ constRef buf
          store pending true

    handler complete "complete" $ callback $ const $ store pending false

hxstreamDecodeTower :: String
                    -> ChanOutput (Stored Uint8)
                    -> ChanInput  CyphertextArray
                    -> Tower e ()
hxstreamDecodeTower n serial_chan ct_chan = do
  towerModule $ H.hxstreamModule
  monitor (n ++ "_datalink_decode") $ do
    monitorModuleDef $ depend H.hxstreamModule
    hx <- stateInit "hx_decoder" H.initStreamState
    a <- airDataHandler
    handler serial_chan "decoder_serial_in" $ do
      e <- emitter ct_chan 1
      callbackV $ \b -> do
        H.decodes [a e] hx b

-- Note this is not strictly safe - it can only actually be used
-- safely by the handler for which the emitter is provided.
airDataHandler :: Monitor e (Emitter CyphertextArray -> H.FrameHandler)
airDataHandler = do
  decodedCtr <- stateInit "airdata_frames_decoded" (ival (0 :: Uint32))
  decoded    <- state     "airdata_decoded"
  overrun    <- state     "airdata_overrun"
  return $ \e -> H.mkFrameHandler H.ScopedFrameHandler
    { H.fhTag = airDataTag
    , H.fhBegin = do
        store overrun false
        arrayMap $ \ix -> store (decoded ! ix) 0
    , H.fhData = \v offs ->
        ifte_ (offs <? fromInteger cyphertextSize)
              (store (decoded ! (toIx offs)) v)
              (store overrun true)
    , H.fhEnd = do
        o <- deref overrun
        unless o $ do
          emit e (constRef decoded)
          decodedCtr %= (+1)
    }
