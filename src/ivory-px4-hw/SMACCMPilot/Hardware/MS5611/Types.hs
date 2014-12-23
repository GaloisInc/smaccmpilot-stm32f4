{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MS5611.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

[ivory|
struct ms5611_calibration
  { coeff1  :: Stored Uint16
  ; coeff2  :: Stored Uint16
  ; coeff3  :: Stored Uint16
  ; coeff4  :: Stored Uint16
  ; coeff5  :: Stored Uint16
  ; coeff6  :: Stored Uint16
  }

struct ms5611_sample
  { sample_temperature :: Stored Uint32
  ; sample_pressure    :: Stored Uint32
  ; sample_time        :: Stored ITime
  }

struct ms5611_measurement
  { initfail   :: Stored IBool
  ; sampfail   :: Stored IBool
  ; pressure    :: Stored IFloat -- mbar
  ; temperature :: Stored IFloat -- deg celsius
  ; time        :: Stored ITime
  }
|]

ms5611TypesModule :: Module
ms5611TypesModule = package "ms5611_types" $ do
  defStruct (Proxy :: Proxy "ms5611_calibration")
  defStruct (Proxy :: Proxy "ms5611_sample")
  defStruct (Proxy :: Proxy "ms5611_measurement")
  depend serializeModule
  incl ms5611PackRef
  incl ms5611UnpackRef

ms5611PackRef :: Def ('[ Ref s1 (CArray (Stored Uint8))
                       , Uint32
                       , ConstRef s2 (Struct "ms5611_measurement")
                       ] :-> () )
ms5611PackRef = proc "ms5611_pack_ref" $ \ buf off msg -> body $ do
  ifail <- deref (msg ~> initfail)
  sfail <- deref (msg ~> sampfail)
  stime <- deref (msg ~> time)
  pack buf (off + 0) (ifail ? ((1 :: Uint8), 0))
  pack buf (off + 1) (sfail ? ((1 :: Uint8), 0))
  packRef buf (off + 2) (msg ~> pressure)
  packRef buf (off + 6) (msg ~> temperature)
  pack buf (off + 10) (toIMicroseconds stime :: Sint64)

ms5611UnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                         , Uint32
                         , Ref s2 (Struct "ms5611_measurement")
                         ] :-> () )
ms5611UnpackRef = proc "ms5611_unpack_ref" $ \ buf off msg -> body $ do
  ifail <- unpack buf (off + 0)
  sfail <- unpack buf (off + 1)
  unpackRef buf (off + 2) (msg ~> pressure)
  unpackRef buf (off + 6) (msg ~> temperature)
  stime <- unpack buf (off + 10)
  store (msg ~> initfail) ((ifail :: Uint8) /=? 0)
  store (msg ~> sampfail) ((sfail :: Uint8) /=? 0)
  store (msg ~> time) (fromIMicroseconds (stime :: Sint64))

instance SerializableRef (Struct "ms5611_measurement") where
  packRef = call_ ms5611PackRef
  unpackRef = call_ ms5611UnpackRef
