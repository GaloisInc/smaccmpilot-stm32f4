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
  wrappedPackMod mpu6000Wrapper

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

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

mpu6000Wrapper :: WrappedPackRep (Struct "mpu6000_sample")
mpu6000Wrapper = wrapPackRep "mpu6000_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel gyro_x
  , packLabel gyro_y
  , packLabel gyro_z
  , packLabel accel_x
  , packLabel accel_y
  , packLabel accel_z
  , packLabel temp
  , packLabel' time packITime
  ]

instance Packable (Struct "mpu6000_sample") where
  packRep = wrappedPackRep mpu6000Wrapper
