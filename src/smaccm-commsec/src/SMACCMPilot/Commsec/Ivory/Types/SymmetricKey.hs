{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Ivory.Types.SymmetricKey where

import Ivory.Language
import Ivory.Tower.Config
import SMACCMPilot.Commsec.Sizes

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

symmetricKeyParser :: ConfigParser (Init (Struct "symmetric_key"))
symmetricKeyParser = subsection "symmetric_key" $ do
  s2c_ks_ <- subsection "server_to_client" ks
  c2s_ks_ <- subsection "client_to_server" ks
  return $ istruct [ s2c_ks .= toiarray s2c_ks_, c2s_ks .= toiarray c2s_ks_ ]
  where
  toiarray as = iarray (map (ival . fromInteger) as)
  ks = subsection "keysalt" (arrayOfLen 24 (boundedInteger 0 255))
  arrayOfLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")
