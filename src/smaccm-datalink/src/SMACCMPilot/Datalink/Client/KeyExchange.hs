

module SMACCMPilot.Datalink.Client.KeyExchange
  ( keyExchangeMode
  , encoder
  , decoder
  ) where

import Pipes
import Control.Concurrent.Async
import qualified GEC.Datagram.Pure as GEC

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Mode

import           Data.ByteString.Char8 (ByteString)

import SMACCMPilot.Commsec.KeyExchange

keyExchangeMode :: DatalinkRole -> PubKey -> PrivKey -> PubKey
                -> IO ( Console -> Poppable ByteString -> Pushable ByteString -> IO (Async ())
                      , Console -> Poppable ByteString -> Pushable ByteString -> IO (Async ())
                      )
keyExchangeMode _role _mypub _mypriv _theirpub = do

  (_decode_sk_push, decode_sk_pop) <- newQueue
  (_encode_sk_push, encode_sk_pop) <- newQueue

  ke_signal_out <- newQueue
  ke_signal_in <- newQueue

  return ( encoder encode_sk_pop (snd ke_signal_out)
         , decoder decode_sk_pop (fst ke_signal_in)
         )

encoder :: Poppable (Maybe ByteString)  -- Symmetric Key
        -> Poppable ByteString          -- outbound ke in-band message
        -> Console
        -> Poppable ByteString -- PT out
        -> Pushable ByteString -- CT out
        -> IO (Async ())
encoder newkey keout console pt_out_frame ct_out_frame =
  asyncRunDLIO console "key exchange encoder" $ aux Nothing
  where
  aux :: Maybe GEC.ContextOut -> DLIO ()
  aux ctx = popSelect
    [ newkey       ==> \k -> case k of
        Nothing -> aux Nothing
        Just key -> do
          ctx' <- mkSKEncoder key
          aux ctx'
    , keout        ==> \msg -> do
        liftIO $ queuePush ct_out_frame msg
        aux ctx
    , pt_out_frame ==> \pt -> case ctx of
        Nothing -> aux ctx
        Just encContext -> do
          case GEC.encode encContext pt of
            Just (ctx', ct) -> do
              liftIO $ queuePush ct_out_frame ct
              aux (Just ctx')
            Nothing -> do
              writeErr "GEC encode failed"
              aux ctx
    ]

  mkSKEncoder :: ByteString -> DLIO (Maybe GEC.ContextOut)
  mkSKEncoder ks = case GEC.mkContextOut GEC.Small ks of
    Just ctx -> do
      writeLog ("created commsecEncoder with ks " ++ show ks)
      return (Just ctx)
    Nothing -> do
      writeErr ("mkSKEncoder: failed to create a GEC decode context from ks " ++ show ks)
      return Nothing

decoder :: Poppable (Maybe ByteString)  -- Symmetric Key
        -> Pushable ByteString          -- forward in-band messages to ke
        -> Console
        -> Poppable ByteString -- CT in
        -> Pushable ByteString -- PT in
        -> IO (Async ())
decoder newkey kein console ct_in_frame pt_in_frame =
  asyncRunDLIO console "key exchange decoder" $ aux Nothing
  where
  aux :: Maybe GEC.ContextIn -> DLIO ()
  aux ctx = popSelect
   [ newkey        ==> \k -> case k of
        Nothing -> aux Nothing
        Just key -> do
          ctx' <- mkSKDecoder key
          aux ctx'
   , ct_in_frame   ==> \ct -> do
        liftIO $ queuePush kein ct
        case ctx of
          Nothing -> aux ctx
          Just decContext -> do
            case GEC.decode decContext ct of
              Just (ctx', pt) -> do
                liftIO $ queuePush pt_in_frame pt
                aux (Just ctx')
              Nothing -> do
                writeErr "GEC decode failed"
                aux ctx
   ]

  mkSKDecoder :: ByteString -> DLIO (Maybe GEC.ContextIn)
  mkSKDecoder ks = case GEC.mkContextIn GEC.Small ks of
    Just ctx -> do
      writeLog ("created commsecDecoder with ks " ++ show ks)
      return (Just ctx)
    Nothing -> do
      writeErr ("mkSKDecoder: failed to create a GEC encode context from ks " ++ show ks)
      return Nothing
