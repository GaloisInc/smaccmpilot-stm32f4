{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Ivory.Types.SymmetricKey where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import Data.Word

[ivory|
struct symmetric_key
  { c2s_ks :: SymKeySaltArray
  ; s2c_ks :: SymKeySaltArray
  }
|]

deriveSymmetricKey :: Def ('[ ConstRef s1 KeyMaterial
                            , Ref s2 (Struct "symmetric_key")]:->())
deriveSymmetricKey = proc "gec_derive_symmetric_key" $ \km sk -> body $ do
    arrayMap $ \ix -> do
      v <- deref (km ! ix)
      let sk_ix :: KeyMaterialIx -> SymKeySaltIx
          sk_ix  = toIx . fromIx
      ifte_ (ix <? sks)
            (store ((sk ~> c2s_ks) ! (sk_ix ix)) v)
            (store ((sk ~> s2c_ks) ! (sk_ix (ix - sks))) v)
  where
  sks :: KeyMaterialIx
  sks = fromIntegral symKeySaltSize

symmetricKeyTypesModule :: Module
symmetricKeyTypesModule = package "gec_symmetric_key_types" $ do
    incl deriveSymmetricKey
    defStruct (Proxy :: Proxy "symmetric_key")

symKeySaltArrayIval :: [Word8] -> Init SymKeySaltArray
symKeySaltArrayIval w8s = iarray (map (ival . fromIntegral) w8s)

symmetricKeyIval :: SymmetricKey -> Init (Struct "symmetric_key")
symmetricKeyIval sk = istruct [ s2c_ks .= symKeySaltArrayIval (sk_s2c sk)
                              , c2s_ks .= symKeySaltArrayIval (sk_c2s sk) ]
