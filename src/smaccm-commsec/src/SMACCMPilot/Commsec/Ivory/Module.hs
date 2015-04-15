{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Commsec.Ivory.Module
  ( -- * Symmetric Key Crypto
    gecEncode
  , GecEncode
  , gec_encode_init
  , gec_encode_run
  , gec_encode_moddef
  , gecDecode
  , GecDecode
  , gec_decode_init
  , gec_decode_run
  , gec_decode_moddef
  -- * Key Exchange
  , gkeInitiate, GkeInitiate
  , gke_initiate_init, gke_initiate, gke_response_ack, gke_panic, gke_initiate_moddef
  , gkeRespond, GkeRespond
  , gke_respond_init, gke_respond , gke_finish, gke_panic_r, gke_respond_moddef
  ) where

import qualified Data.ByteString as B
import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Ivory.Import

--------------------------------------------------------------------------------
-- Begin Symmetric Encryption Modules
--------------------------------------------------------------------------------

data GecEncode =
   GecEncode
    { gec_encode_init       :: forall s1 eff . ConstRef s1 SymKeySaltArray
                                             ->  Ivory eff ()
    , gec_encode_run        :: forall s1 s2 eff  . ConstRef s1 PlaintextArray
                                                -> Ref       s2 CyphertextArray
                                                -> Ivory eff GecError
    , gec_encode_moddef     :: ModuleDef
    }

gecEncode :: String -> GecEncode
gecEncode n = GecEncode
  { gec_encode_init    = call_ init_proc
  , gec_encode_run     = call  run_proc
  , gec_encode_moddef  = do
      defMemArea sym_key_area
      incl gec_init_key
      incl gec_encrypt
      incl init_proc
      incl run_proc
  }
  where
  named nn = n ++ "_" ++ nn
  sym_key_area = area (named "global_gec_sym_key_enc") Nothing
  sym_key = addrOf sym_key_area

  init_proc :: Def('[ConstRef s1 SymKeySaltArray]:->())
  init_proc = proc (named "encode_init") $ \kref -> body $ do
    call_ gec_init_key sym_key kref

  run_proc :: Def('[ ConstRef s1 PlaintextArray , Ref s2 CyphertextArray
                   ] :-> GecError)
  run_proc = proc (named "encode_run") $ \pt ct -> body $ do
        r <- call gec_encrypt sym_key pt ct
        ret r

data GecDecode =
  GecDecode
    { gec_decode_init   :: forall s1 eff . ConstRef s1 SymKeySaltArray
                                         -> Ivory eff ()
    , gec_decode_run    :: forall s1 s2 eff . ConstRef s1 CyphertextArray
                                            -> Ref      s2 PlaintextArray
                                            -> Ivory eff GecError
    , gec_decode_moddef :: ModuleDef
    }

gecDecode :: String -> GecDecode
gecDecode n = GecDecode
  { gec_decode_init    = call_ init_proc
  , gec_decode_run     = call  run_proc
  , gec_decode_moddef  = do
      defMemArea sym_key_area
      incl gec_init_key
      incl gec_decrypt
      incl init_proc
      incl run_proc
  }
  where
  named nn = n ++ "_" ++ nn
  sym_key_area = area (named "global_gec_sym_key_dec") Nothing
  sym_key = addrOf sym_key_area

  init_proc :: Def('[ConstRef s1 SymKeySaltArray]:->())
  init_proc = proc (named "decode_init") $ \kref -> body $ do
    call_ gec_init_key sym_key kref

  run_proc :: Def('[ ConstRef s1 CyphertextArray, Ref s2 PlaintextArray
                   ]:->GecError)
  run_proc = proc (named "decode_run") $ \const_ct pt -> body $ do
    r <- call gec_decrypt sym_key const_ct pt
    ret r

--------------------------------------------------------------------------------
-- Begin Key Exchange Modules
--------------------------------------------------------------------------------

data GkeInitiate =
    GkeInitiate { gke_initiate_init :: forall eff
                                     . Ivory eff ()
                , gke_initiate :: forall s1 s2 eff
                                .  Ref s1 GecKeMessage1
                                -> ConstRef s2  GecKeRandomData
                                -> Ivory eff GecError
                , gke_response_ack :: forall s1 s2 s3 eff
                                .  ConstRef s1 GecKeMessage2
                                -> Ref s2 GecKeMessage3
                                -> Ref s3 KeyMaterial
                                -> Ivory eff GecError
                , gke_panic :: forall eff . Ivory eff ()
                , gke_initiate_moddef :: ModuleDef
           }

-- | Construct a key exchange initiator with hard-coded public and private
-- keys.
gkeInitiate :: String -> B.ByteString -> (B.ByteString, B.ByteString) -> GkeInitiate
gkeInitiate n themP (meP, meQ) = GkeInitiate
  { gke_initiate_init   = call_ init_proc
  , gke_initiate        = call  run_gke_initiate
  , gke_response_ack    = call  run_gke_response_ack
  , gke_panic           = call_ run_gke_panic
  , gke_initiate_moddef = do
      defMemArea sts_ctx_area
      incl gke_initiate_sts
      incl gke_response_ack_sts
      incl gke_clear_ctx
      incl gke_init
      incl gec_mk_pubkey
      incl gec_mk_privkey
      incl init_proc
      incl run_gke_initiate
      incl run_gke_response_ack
      incl run_gke_panic
  }
  where
  named nn = n ++ "_" ++ nn
  sts_ctx_area = area (named "global_gke_sts_ctx_initiate") Nothing
  sts_ctx = addrOf sts_ctx_area

  init_proc :: Def('[]:->())
  init_proc = proc (named "initiator_init") $ body $ do
    meQRef   <- local meQ'
    mePRef   <- local meP'
    themPRef <- local themP'
    mePriv   <- local izero
    mePub    <- local izero
    themPub  <- local izero
    call_ gec_mk_privkey mePriv  (constRef meQRef) (constRef mePRef)
    call_ gec_mk_pubkey mePub    (constRef mePRef)
    call_ gec_mk_pubkey themPub  (constRef themPRef)
    call_ gke_init sts_ctx (constRef mePub) (constRef mePriv) (constRef themPub)

  run_gke_initiate :: Def('[ Ref s1 GecKeMessage1, ConstRef s2 GecKeRandomData
                   ]:->GecError)
  run_gke_initiate = proc (named "gke_initiate") $ \msgBuf rand -> body $ do
      r <- call gke_initiate_sts msgBuf sts_ctx rand
      ret r

  run_gke_response_ack :: Def('[ ConstRef s1 GecKeMessage2, Ref s2 GecKeMessage3, Ref s3 KeyMaterial
                   ]:->GecError)
  run_gke_response_ack = proc (named "gke_response_ack") $ \m2 m3 keymat -> body $ do
      r <- call gke_response_ack_sts m2 m3 sts_ctx keymat
      ret r

  run_gke_panic :: Def('[]:->())
  run_gke_panic = proc (named "gke_clear") $ body $ call_ gke_clear_ctx sts_ctx

  meQ'   = iarray (map (ival . fromIntegral) (B.unpack meQ))
  meP'   = iarray (map (ival . fromIntegral) (B.unpack meP))
  themP' = iarray (map (ival . fromIntegral) (B.unpack themP))

data GkeRespond =
     GkeRespond { gke_respond_init :: forall eff
                                . Ivory eff ()
                , gke_respond :: forall s1 s2 s3 eff
                                .  ConstRef s1 GecKeMessage1
                                -> Ref s2 GecKeMessage2
                                -> ConstRef s3 GecKeRandomData
                                -> Ivory eff GecError
                , gke_finish :: forall s1 s2 eff
                                .  ConstRef s1 GecKeMessage3
                                -> Ref s2 KeyMaterial
                                -> Ivory eff GecError
                , gke_respond_moddef :: ModuleDef
                , gke_panic_r :: forall eff . Ivory eff ()
            }

-- | Construct a key exchange responder with hard-coded public and private
-- keys.
gkeRespond :: String -> B.ByteString -> (B.ByteString, B.ByteString) -> GkeRespond
gkeRespond n themP (meP, meQ) = GkeRespond
  { gke_respond_init     = call_ init_proc
  , gke_respond          = call  run_gke_respond
  , gke_finish           = call  run_gke_finish
  , gke_panic_r          = call_ run_gke_panic
  , gke_respond_moddef   = do
      defMemArea sts_ctx_area
      incl gke_respond_sts
      incl gke_finish_sts
      incl gke_clear_ctx
      incl gke_init
      incl gec_mk_pubkey
      incl gec_mk_privkey
      incl init_proc
      incl run_gke_panic
      incl run_gke_finish
      incl run_gke_respond
  }
  where
  named nn = n ++ "_" ++ nn
  sts_ctx_area = area (named "global_gke_sts_ctx_response") Nothing
  sts_ctx = addrOf sts_ctx_area

  init_proc :: Def('[]:->())
  init_proc = proc (named "respond_init") $ body $ do
    meQRef   <- local meQ'
    mePRef   <- local meP'
    themPRef <- local themP'
    mePriv   <- local izero
    mePub    <- local izero
    themPub  <- local izero
    call_ gec_mk_privkey mePriv  (constRef meQRef) (constRef mePRef)
    call_ gec_mk_pubkey mePub    (constRef mePRef)
    call_ gec_mk_pubkey themPub  (constRef themPRef)
    call_ gke_init sts_ctx (constRef mePub) (constRef mePriv) (constRef themPub)

  run_gke_respond :: Def('[ConstRef s1 GecKeMessage1, Ref s2 GecKeMessage2, ConstRef s3 GecKeRandomData
                   ]:->GecError)
  run_gke_respond = proc (named "gke_respond") $ \m1 m2 rand -> body $ do
      r <- call gke_respond_sts m1 m2 sts_ctx rand
      ret r

  run_gke_finish :: Def('[ConstRef s1 GecKeMessage3, Ref s2 KeyMaterial
                   ]:->GecError)
  run_gke_finish = proc (named "gke_finish") $ \m3 keymat -> body $ do
      r <- call gke_finish_sts m3 sts_ctx keymat
      ret r

  run_gke_panic :: Def('[]:->())
  run_gke_panic = proc (named "gke_clear") $ body $ call_ gke_clear_ctx sts_ctx

  meQ'   = iarray (map (ival . fromIntegral) (B.unpack meQ))
  meP'   = iarray (map (ival . fromIntegral) (B.unpack meP))
  themP' = iarray (map (ival . fromIntegral) (B.unpack themP))
