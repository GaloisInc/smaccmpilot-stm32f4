{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Commsec.Module
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
import Ivory.Stdlib (arrayCopy, when)
import qualified SMACCMPilot.Communications  as C
import SMACCMPilot.Commsec.Config
import SMACCMPilot.Commsec.Import
import SMACCMPilot.Commsec.Import.Types ()
import SMACCMPilot.Commsec.Error

headerLen :: Integer
headerLen = 8

data CommsecEncode =
  CommsecEncode
    { commsec_encode_init   :: forall eff . Ivory eff ()
    , commsec_encode_run    :: forall s1 s2 eff . ConstRef s1 C.PlaintextArray
                                               -> Ref s2 C.CyphertextArray
                                               -> Ivory eff CommsecError
    , commsec_encode_moddef :: ModuleDef
    }

commsecEncode :: Config -> String -> CommsecEncode
commsecEncode c n = CommsecEncode
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
    call_ securePkg_init_enc encode_ctx i s kref

  i = fromIntegral (encode_id c)
  k = iarray (map (ival . fromIntegral) (encode_key c))
  s = fromIntegral (encode_salt c)

  run_proc :: Def('[ ConstRef s1 C.PlaintextArray , Ref s2 C.CyphertextArray
                   ]:->CommsecError)
  run_proc = proc (named "decode_run") $ \pt ct -> body $ do
    -- Copy pt into ct, offset by headerLen
    arrayCopy ct pt (fromIntegral headerLen) (arrayLen pt)
    -- Encode ct in place
    r <- call securePkg_enc_in_place encode_ctx ct
              (fromIntegral headerLen) (fromIntegral C.cyphertextSize)
    -- Return error code
    ret r

data CommsecDecode =
  CommsecDecode
    { commsec_decode_init   :: forall eff . Ivory eff ()
    , commsec_decode_run    :: forall s1 s2 eff . ConstRef s1 C.CyphertextArray
                                               -> Ref s2 C.PlaintextArray
                                               -> Ivory eff CommsecError
    , commsec_decode_moddef :: ModuleDef
    }

commsecDecode :: Config -> String -> CommsecDecode
commsecDecode c n = CommsecDecode
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

  k = iarray (map (ival . fromIntegral) (decode_key c))
  s = fromIntegral (decode_salt c)

  run_proc :: Def('[ ConstRef s1 C.CyphertextArray, Ref s2 C.PlaintextArray
                   ]:->CommsecError)
  run_proc = proc (named "decode_run") $ \ct pt -> body $ do
    arrayMap $ \(ix :: C.CyphertextIx) ->
      when (ix >=? hlen .&& ix <? arrayLen pt) $ do
        v <- deref (ct ! ix)
        store (pt ! ptIx ix) v

    ret succeed
  hlen = fromInteger headerLen
  ptIx :: C.CyphertextIx -> C.PlaintextIx
  ptIx = toIx . (\x -> x - fromInteger headerLen) .  fromIx

