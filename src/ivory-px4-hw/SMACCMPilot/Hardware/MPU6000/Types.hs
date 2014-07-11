{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MPU6000.Types where

import Ivory.Language
import Ivory.Tower.Types.Time

mpu6000TypesModule :: Module
mpu6000TypesModule = package "mpu6000_types" $ do
  defStruct (Proxy :: Proxy "mpu6000_sample")

[ivory|
struct mpu6000_sample
  { valid     :: Stored IBool
  ; time      :: Stored ITime
  ; gyro_x    :: Stored IFloat -- degrees/sec
  ; gyro_y    :: Stored IFloat -- degrees/sec
  ; gyro_z    :: Stored IFloat -- degrees/sec
  ; accel_x   :: Stored IFloat -- m/s/s
  ; accel_y   :: Stored IFloat -- m/s/s
  ; accel_z   :: Stored IFloat -- m/s/s
  ; temp      :: Stored IFloat -- degrees Celsius
  }
|]

