{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Datalink.HXStream.Tower
  ( hxstreamEncodeTower
  , hxstreamEncodeTower'
  , HXStreamHandler()
  , hxstreamHandler
  , hxstreamHandler'
  , hxstreamDecodeTower
  , HXCyphertext
  , airDataEncodeTower
  , airDataDecodeTower
  ) where

import Control.Monad (forM)
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.RingBuffer

import qualified SMACCMPilot.Datalink.HXStream.Ivory as H
import SMACCMPilot.Commsec.Sizes

hxstreamEncodeTower :: forall str msg len pop_period buf_size e
                     . (IvoryString str, Packable msg, IvoryArea msg, IvoryZero msg, ANat len, Time pop_period, ANat buf_size)
                    => String
                    -> ChanOutput msg
                    -> Proxy len
                    -> H.Tag
                    -> BackpressureTransmit str ('Stored IBool)
                    -> pop_period
                    -> Proxy buf_size
                    -> Tower e ()
hxstreamEncodeTower n = hxstreamEncodeTower' n packRep


hxstreamEncodeTower' :: forall str msg len pop_period buf_size e
                      . (IvoryString str, IvoryArea msg, IvoryZero msg, ANat len, Time pop_period, ANat buf_size)
                     => String
                     -> PackRep msg
                     -> ChanOutput msg
                     -> Proxy len
                     -> H.Tag
                     -> BackpressureTransmit str ('Stored IBool)
                     -> pop_period
                     -> Proxy buf_size
                     -> Tower e ()
hxstreamEncodeTower' n rep ct_chan buflen tag (BackpressureTransmit serial_chan complete) pop_period _buf_size = do
  let deps = [H.hxstreamModule, serializeModule]
  mapM_ towerModule deps
  mapM_ towerArtifact serializeArtifacts

  p <- period pop_period
  ct_chan_buf <- channel

  monitor (n ++ "_datalink_encode") $ do
    monitorModuleDef $ mapM_ depend deps
    (rb :: RingBuffer buf_size msg) <- monitorRingBuffer "datalink_encode"

    pending <- state "pending"

    handler ct_chan "encoder_ct_push" $ do
      callback $ \msg -> do
        _ <- ringbuffer_push rb msg
        -- dropped message!
        return ()

    handler p "periodic_encoder_ct_pop" $ do
      e <- emitter (fst ct_chan_buf) 1
      callback $ const $ do
        already_pending <- deref pending
        unless already_pending $ do
          msg <- local izero
          got <- ringbuffer_pop rb msg
          ifte_ got (emit e (constRef msg)) (return ())

    handler (snd ct_chan_buf) "encoder_ct_output" $ do
      e <- emitter serial_chan 1
      callback $ \ msg -> do
        ct <- local (izerolen buflen)
        packInto' rep ct 0 msg
        buf <- local izero
        H.encodeString tag (constRef ct) buf
        emit e $ constRef buf
        store pending true

    handler complete "complete" $ callback $ const $ store pending false

data HXStreamHandler = forall msg. (IvoryArea msg, IvoryZero msg) => HXStreamHandler
  { handlerTag :: H.Tag
  , handlerPackRep :: PackRep msg
  , handlerChan :: ChanInput msg
  }

hxstreamHandler :: (Packable msg, IvoryArea msg, IvoryZero msg) => H.Tag -> ChanInput msg -> HXStreamHandler
hxstreamHandler t c = hxstreamHandler' t packRep c

hxstreamHandler' :: (IvoryArea msg, IvoryZero msg) => H.Tag -> PackRep msg -> ChanInput msg -> HXStreamHandler
hxstreamHandler' = HXStreamHandler

hxstreamDecodeTower :: ANat len
                    => String
                    -> ChanOutput ('Stored Uint8)
                    -> Proxy len
                    -> [HXStreamHandler]
                    -> Tower e ()
hxstreamDecodeTower n serial_chan buflen handlers = do
  let deps = [H.hxstreamModule, serializeModule]
  mapM_ towerDepends deps
  mapM_ towerModule  deps
  mapM_ towerArtifact serializeArtifacts

  monitor (n ++ "_datalink_decode") $ do
    monitorModuleDef $ depend H.hxstreamModule
    hx <- stateInit "hx_decoder" H.initStreamState
    buf <- stateInit "buf" (izerolen buflen)
    overrun <- state "overrun"

    handler serial_chan "decoder_serial_in" $ do
      emitters <- forM handlers $ \ HXStreamHandler { .. } -> do
        let needed = packSize handlerPackRep * 2 + 3
        e <- if needed <= arrayLen buf
          then emitter handlerChan 1
          else fail $
            "handler needs buffer length at least " ++ show needed ++
            " but was only given " ++ show (arrayLen buf :: Int) ++ " bytes"

        return $ H.mkFrameHandler H.ScopedFrameHandler
          { H.fhTag = handlerTag
          , H.fhBegin = do
              store overrun false
              arrayMap $ \ix -> store (buf ! ix) 0
          , H.fhData = \v offs ->
              -- by transitivity of (<), offs < arrayLen buf
              ifte_ (offs <? fromInteger needed)
                    (store (buf ! toIx offs) v)
                    (store overrun true)
          , H.fhEnd = do
              o <- deref overrun
              unless o $ do
                decoded <- local izero
                unpackFrom' handlerPackRep (constRef buf) 0 decoded
                emit e (constRef decoded)
          }

      callbackV $ H.decodes emitters hx

-- Convenience functions for old users of this module.
-- hxstream{Encode,Decode}Tower are renamed to airData{Encode,Decode}Tower
-- so the more general names can be used for more general functions.

airDataTag :: H.Tag
airDataTag = 0

[ivory| string struct HXCyphertext 195 |] -- 96*2+3==195

airDataEncodeTower :: (Time pop_period, ANat buf_size)
                   => String
                   -> ChanOutput CyphertextArray
                   -> BackpressureTransmit HXCyphertext ('Stored IBool)
                   -> pop_period
                   -> Proxy buf_size
                   -> Tower e ()
airDataEncodeTower n ct_chan tx pop_period _buf_size = do
  let bufmod = package "hx_cyphertext" $
                 defStringType (Proxy :: Proxy HXCyphertext)
  towerModule bufmod
  towerDepends bufmod
  hxstreamEncodeTower
    n ct_chan (Proxy :: Proxy 96) airDataTag tx pop_period _buf_size

airDataDecodeTower :: String
                   -> ChanOutput ('Stored Uint8)
                   -> ChanInput  CyphertextArray
                   -> Tower e ()
airDataDecodeTower n serial_chan ct_chan = do
  hxstreamDecodeTower
    n serial_chan (Proxy :: Proxy 195) [hxstreamHandler airDataTag ct_chan]
