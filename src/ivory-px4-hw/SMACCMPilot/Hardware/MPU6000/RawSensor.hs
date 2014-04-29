{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MPU6000.RawSensor where

import Ivory.Language
import Ivory.Tower.Types.Time

rawSensorTypeModule :: Module
rawSensorTypeModule = package "mpu6000_raw_sensor_type" $ do
  defStruct (Proxy :: Proxy "mpu6000_raw_sensor")

[ivory|
struct mpu6000_raw_sensor
  { valid     :: Stored IBool
  ; time      :: Stored ITime
  ; gyro_x    :: Stored Uint16
  ; gyro_y    :: Stored Uint16
  ; gyro_z    :: Stored Uint16
  ; accel_x   :: Stored Uint16
  ; accel_y   :: Stored Uint16
  ; accel_z   :: Stored Uint16
  ; temp      :: Stored Uint16
  }
|]

