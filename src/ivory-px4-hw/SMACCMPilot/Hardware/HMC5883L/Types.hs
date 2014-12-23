{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.HMC5883L.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

hmc5883lTypesModule :: Module
hmc5883lTypesModule = package "hmc5883l_types" $ do
  defStruct (Proxy :: Proxy "hmc5883l_sample")
  depend serializeModule
  incl hmc5883lPackRef
  incl hmc5883lUnpackRef

[ivory|
struct hmc5883l_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- Gauss
  ; time       :: Stored ITime
  }
|]

hmc5883lPackRef :: Def ('[ Ref s1 (CArray (Stored Uint8))
                         , Uint32
                         , ConstRef s2 (Struct "hmc5883l_sample")
                         ] :-> () )
hmc5883lPackRef = proc "hmc5883l_pack_ref" $ \ buf off msg -> body $ do
  ifail <- deref (msg ~> initfail)
  sfail <- deref (msg ~> samplefail)
  stime <- deref (msg ~> time)
  pack buf (off + 0) (ifail ? ((1 :: Uint8), 0))
  pack buf (off + 1) (sfail ? ((1 :: Uint8), 0))
  packRef buf (off + 2) ((msg ~> sample) ! 0)
  packRef buf (off + 6) ((msg ~> sample) ! 1)
  packRef buf (off + 10) ((msg ~> sample) ! 2)
  pack buf (off + 14) (toIMicroseconds stime :: Sint64)

hmc5883lUnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                           , Uint32
                           , Ref s2 (Struct "hmc5883l_sample")
                           ] :-> () )
hmc5883lUnpackRef = proc "hmc5883l_unpack_ref" $ \ buf off msg -> body $ do
  ifail <- unpack buf (off + 0)
  sfail <- unpack buf (off + 1)
  unpackRef buf (off + 2) ((msg ~> sample) ! 0)
  unpackRef buf (off + 6) ((msg ~> sample) ! 1)
  unpackRef buf (off + 10) ((msg ~> sample) ! 2)
  stime <- unpack buf (off + 14)
  store (msg ~> initfail) ((ifail :: Uint8) /=? 0)
  store (msg ~> samplefail) ((sfail :: Uint8) /=? 0)
  store (msg ~> time) (fromIMicroseconds (stime :: Sint64))

instance SerializableRef (Struct "hmc5883l_sample") where
  packRef = call_ hmc5883lPackRef
  unpackRef = call_ hmc5883lUnpackRef
