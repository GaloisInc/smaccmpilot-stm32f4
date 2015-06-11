
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors
  ( sensorTower
  ) where

import Ivory.Tower

import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Hardware.SensorManager

sensorTower :: (e -> FlightPlatform)
            -> ControllableVehicleAttrs Attr
            -> Tower e ()
sensorTower tofp attrs = do

  (a,g,m,b) <- sensorManager (fp_sensors . tofp) (fp_clockconfig . tofp)

  monitor "sensorAttrs" $ do

    handler a "accel_attr_proxy" $ do
      e <- attrEmitter (accelOutput attrs)
      callback $ \v -> emit e v

    handler g "gyro_raw_attr_proxy" $ do
      e <- attrEmitter (gyroRawOutput attrs)
      callback $ \v -> emit e v

    handler m "mag_raw_attr_proxy" $ do
      e <- attrEmitter (magRawOutput attrs)
      callback $ \v -> emit e v

    handler b "baro_attr_proxy" $ do
      e <- attrEmitter (baroOutput attrs)
      callback $ \v -> emit e v

