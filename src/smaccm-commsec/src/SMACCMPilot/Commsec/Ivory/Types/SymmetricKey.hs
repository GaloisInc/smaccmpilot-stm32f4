{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SMACCMPilot.Commsec.Ivory.Types.SymmetricKey where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes

[ivory|
struct symmetric_key {
    c2s_ks :: SymKeySaltArray ;
    s2c_ks :: SymKeySaltArray
}
|]

deriveSymmetricKey :: Def ('[ ConstRef s1 KeyMaterial
                            , Ref s2 (Struct "symmetric_key")]:->())
deriveSymmetricKey = proc "gec_derive_symmetric_key" $ \km sk -> body $ do
    return () -- XXX TODO

symmetricKeyTypesModule :: Module
symmetricKeyTypesModule = package "gec_symmetric_key_types" $ do
    incl deriveSymmetricKey
    defStruct (Proxy :: Proxy "symmetric_key")
