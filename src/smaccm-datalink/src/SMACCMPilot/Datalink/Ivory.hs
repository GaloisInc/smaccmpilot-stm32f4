{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Datalink.Ivory
  ( encodeMonitor
  , decodeMonitor
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import qualified Ivory.HXStream as H
import SMACCMPilot.Commsec.Sizes

airDataTag :: H.Tag
airDataTag = 0

encodeMonitor :: String
              -> ChanOutput CyphertextArray
              -> ChanInput  (Stored Uint8)
              -> Tower e ()
encodeMonitor n ct_chan serial_chan = monitor (n ++ "_datalink_encode") $ do
  monitorModuleDef $ depend H.hxstreamModule
  handler ct_chan "encoder_ct_in" $ do
    e <- emitter serial_chan (2*cyphertextSize + 3)
    callback $ \ct -> do
      H.encode airDataTag ct (emitV e)

decodeMonitor :: String
              -> ChanOutput (Stored Uint8)
              -> ChanInput  CyphertextArray
              -> Tower e ()
decodeMonitor n serial_chan ct_chan = monitor (n ++ "_datalink_decode") $ do
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
