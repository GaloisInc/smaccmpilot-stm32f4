{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Commsec.Ivory.Module
  ( gecEncode
  , GecEncode
  , gec_encode_init
  , gec_encode_run
  , gec_encode_moddef
  , gecDecode
  , GecDecode
  , gec_decode_init
  , gec_decode_run
  , gec_decode_moddef
  ) where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Ivory.Import
import SMACCMPilot.Commsec.Ivory.Import.Types ()

data GecEncode =
   GecEncode
    { gec_encode_init       :: -- XXX static keys still forall s1 eff . Ref s1 KeyAndSaltArray ->  Ivory eff ()
                                             forall eff. Ivory eff ()
    , gec_encode_run        :: forall s1 s2 eff  . ConstRef s1 PlaintextArray
                                                -> Ref       s2 CyphertextArray
                                                -> Ivory eff GecError
    , gec_encode_moddef     :: ModuleDef
    }

gecEncode :: KeySalt -> String -> GecEncode
gecEncode ks n = GecEncode
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

  init_proc :: Def('[]:->())
  init_proc = proc (named "encode_init") $ body $ do
    kref <- local k
    call_ gec_init_key sym_key (constRef kref) -- XXX GecError?

  k = iarray (map (ival . fromIntegral) (ks_keysalt ks)) :: Init KeyAndSaltArray -- Init ('Array 28 (Stored Uint8)) -- XXX TMD explicit signature now required?

  run_proc :: Def('[ ConstRef s1 PlaintextArray , Ref s2 CyphertextArray
                   ] :-> GecError)
  run_proc = proc (named "encode_run") $ \pt ct -> body $ do
        r <- call gec_encrypt sym_key pt ct
        ret r

data GecDecode =
  GecDecode
    { gec_decode_init   :: forall eff . Ivory eff ()
    , gec_decode_run    :: forall s1 s2 eff . ConstRef s1 CyphertextArray
                                            -> Ref      s2 PlaintextArray
                                            -> Ivory eff GecError
    , gec_decode_moddef :: ModuleDef
    }

gecDecode :: KeySalt -> String -> GecDecode
gecDecode ks n = GecDecode
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

  init_proc :: Def('[]:->())
  init_proc = proc (named "decode_init") $ body $ do
    kref <- local k
    call_ gec_init_key sym_key (constRef kref) -- XXX GecError

  k = iarray (map (ival . fromIntegral) (ks_keysalt ks)) :: Init KeyAndSaltArray -- ('Array 28 (Stored Uint8)) -- XXX TMD explicit signature now required?

  run_proc :: Def('[ ConstRef s1 CyphertextArray, Ref s2 PlaintextArray
                   ]:->GecError)
  run_proc = proc (named "decode_run") $ \const_ct pt -> body $ do
    r <- call gec_decrypt sym_key const_ct pt
    ret r
