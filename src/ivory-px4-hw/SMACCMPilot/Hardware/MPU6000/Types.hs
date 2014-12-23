{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MPU6000.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

mpu6000TypesModule :: Module
mpu6000TypesModule = package "mpu6000_types" $ do
  defStruct (Proxy :: Proxy "mpu6000_sample")
  depend serializeModule
  incl mpu6000PackRef
  incl mpu6000UnpackRef

[ivory|
struct mpu6000_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; gyro_x     :: Stored IFloat -- degrees/sec
  ; gyro_y     :: Stored IFloat -- degrees/sec
  ; gyro_z     :: Stored IFloat -- degrees/sec
  ; accel_x    :: Stored IFloat -- m/s/s
  ; accel_y    :: Stored IFloat -- m/s/s
  ; accel_z    :: Stored IFloat -- m/s/s
  ; temp       :: Stored IFloat -- degrees Celsius
  ; time       :: Stored ITime
  }
|]

mpu6000PackRef :: Def ('[ Ref s1 (CArray (Stored Uint8))
                        , Uint32
                        , ConstRef s2 (Struct "mpu6000_sample")
                        ] :-> () )
mpu6000PackRef = proc "mpu6000_pack_ref" $ \ buf off msg -> body $ do
  ifail <- deref (msg ~> initfail)
  sfail <- deref (msg ~> samplefail)
  stime <- deref (msg ~> time)
  pack buf (off + 0) (ifail ? ((1 :: Uint8), 0))
  pack buf (off + 1) (sfail ? ((1 :: Uint8), 0))
  packRef buf (off + 2) (msg ~> gyro_x)
  packRef buf (off + 6) (msg ~> gyro_y)
  packRef buf (off + 10) (msg ~> gyro_z)
  packRef buf (off + 14) (msg ~> accel_x)
  packRef buf (off + 18) (msg ~> accel_y)
  packRef buf (off + 22) (msg ~> accel_z)
  packRef buf (off + 26) (msg ~> temp)
  pack buf (off + 30) (toIMicroseconds stime :: Sint64)

mpu6000UnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                          , Uint32
                          , Ref s2 (Struct "mpu6000_sample")
                          ] :-> () )
mpu6000UnpackRef = proc "mpu6000_unpack_ref" $ \ buf off msg -> body $ do
  ifail <- unpack buf (off + 0)
  sfail <- unpack buf (off + 1)
  unpackRef buf (off + 2) (msg ~> gyro_x)
  unpackRef buf (off + 6) (msg ~> gyro_y)
  unpackRef buf (off + 10) (msg ~> gyro_z)
  unpackRef buf (off + 14) (msg ~> accel_x)
  unpackRef buf (off + 18) (msg ~> accel_y)
  unpackRef buf (off + 22) (msg ~> accel_z)
  unpackRef buf (off + 26) (msg ~> temp)
  stime <- unpack buf (off + 30)
  store (msg ~> initfail) ((ifail :: Uint8) /=? 0)
  store (msg ~> samplefail) ((sfail :: Uint8) /=? 0)
  store (msg ~> time) (fromIMicroseconds (stime :: Sint64))

instance SerializableRef (Struct "mpu6000_sample") where
  packRef = call_ mpu6000PackRef
  unpackRef = call_ mpu6000UnpackRef
