{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Commsec.Ivory.Module
  ( commsecEncode
  , CommsecEncode
  , commsec_encode_init
  , commsec_encode_run
  , commsec_encode_moddef
  , commsecDecode
  , CommsecDecode
  , commsec_decode_init
  , commsec_decode_run
  , commsec_decode_moddef
  ) where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Config
import SMACCMPilot.Commsec.Ivory.Import
import SMACCMPilot.Commsec.Ivory.Import.Types ()
import SMACCMPilot.Commsec.Ivory.Error

data CommsecEncode =
  CommsecEncode
    { commsec_encode_init   :: forall eff . Ivory eff ()
    , commsec_encode_run    :: forall s1 s2 eff . ConstRef s1 PlaintextArray
                                               -> Ref      s2 CyphertextArray
                                               -> Ivory eff CommsecError
    , commsec_encode_moddef :: ModuleDef
    }

commsecEncode :: KeySalt -> Integer -> String -> CommsecEncode
commsecEncode ks eid n = CommsecEncode
  { commsec_encode_init    = call_ init_proc
  , commsec_encode_run     = call  run_proc
  , commsec_encode_moddef  = do
      inclHeader securePkg_header
      defMemArea encode_ctx_area
      incl init_proc
      incl run_proc
  }
  where
  named nn = n ++ "_" ++ nn
  encode_ctx_area = area (named "encode_ctx") Nothing
  encode_ctx = addrOf encode_ctx_area

  init_proc :: Def('[]:->())
  init_proc = proc (named "encode_init") $ body $ do
    kref <- local k
    assert (i >=? 0 .&& i <? 16)
    call_ securePkg_init_enc encode_ctx i s kref

  i = fromIntegral eid
  k = iarray (map (ival . fromIntegral) (ks_key ks))
  s = fromIntegral (ks_salt ks)

  run_proc :: Def('[ ConstRef s1 PlaintextArray , Ref s2 CyphertextArray
                   ] :-> CommsecError)
  run_proc = proc (named "encode_run") $ \pt ct -> body $ do
        r <- call securePkg_encode encode_ctx pt ct
        ret r

data CommsecDecode =
  CommsecDecode
    { commsec_decode_init   :: forall eff . Ivory eff ()
    , commsec_decode_run    :: forall s1 s2 eff . ConstRef s1 CyphertextArray
                                               -> Ref      s2 PlaintextArray
                                               -> Ivory eff CommsecError
    , commsec_decode_moddef :: ModuleDef
    }

commsecDecode :: KeySalt -> String -> CommsecDecode
commsecDecode ks n = CommsecDecode
  { commsec_decode_init    = call_ init_proc
  , commsec_decode_run     = call  run_proc
  , commsec_decode_moddef  = do
      inclHeader securePkg_header
      defMemArea decode_ctx_area
      incl init_proc
      incl run_proc
  }
  where
  named nn = n ++ "_" ++ nn
  decode_ctx_area = area (named "decode_ctx") Nothing
  decode_ctx = addrOf decode_ctx_area

  init_proc :: Def('[]:->())
  init_proc = proc (named "decode_init") $ body $ do
    kref <- local k
    call_ securePkg_init_dec decode_ctx s kref

  k = iarray (map (ival . fromIntegral) (ks_key ks))
  s = fromIntegral (ks_salt ks)

  run_proc :: Def('[ ConstRef s1 CyphertextArray, Ref s2 PlaintextArray
                   ]:->CommsecError)
  run_proc = proc (named "decode_run") $ \const_ct pt -> body $ do
    r <- call securePkg_decode decode_ctx const_ct pt
    ret r
