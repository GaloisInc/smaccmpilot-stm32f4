
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors
  ( sensorTower
  ) where

import Ivory.Language
import Ivory.Tower

import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Hardware.SensorManager
import           SMACCMPilot.INS.Bias.Gyro.Tower
import           SMACCMPilot.INS.Bias.Magnetometer.Tower
import           SMACCMPilot.INS.Bias.Calibration

sensorTower :: (e -> FlightPlatform)
            -> ControllableVehicleAttrs Attr
            -> Tower e ()
sensorTower tofp attrs = do

  (a,g,m,b) <- sensorManager (fp_sensors . tofp) (fp_clockconfig . tofp)

  -- Accel: no calibration at this time.
  attrProxy (accelOutput attrs) a
  -- Baro: no calibration at this time.
  attrProxy (baroOutput attrs) b

  -- Gyro: there are four attributes we care about.
  --    gyroRawOutput: the uncalibrated gyro sample, from sensorManager
  --    gyroCalibration: the most recent calibration value calculated, from
  --      calcGyroBias.
  --    gyroOutputCalibration: the calibration value, selected by applyCal
  --      applyCal will only choose a new calibration value when the
  --      control law indicated the vehicle is disarmed.
  --    gyroOutput: the calibrated gyro sample, from applyGyroBias

  attrProxy (gyroRawOutput attrs) g

  gyro_bias <- calcGyroBiasTower g a
  attrProxy (gyroCalibration attrs) gyro_bias

  let cl_chan = attrReaderChan (controlLaw attrs)
  (gyro_out, gyro_out_bias) <- applyCalibrationTower gyroCalibrate g gyro_bias cl_chan
  attrProxy (gyroOutput            attrs) gyro_out
  attrProxy (gyroOutputCalibration attrs) gyro_out_bias


  -- Mag: same basic idea as Gyro. We calculate calibration differently, of
  -- course.
  attrProxy (magRawOutput attrs) m

  mag_bias <- calcMagBiasTower m
  attrProxy (magCalibration attrs) mag_bias

  (mag_out, mag_out_bias) <- applyCalibrationTower magCalibrate m mag_bias cl_chan
  attrProxy (magOutput            attrs) mag_out
  attrProxy (magOutputCalibration attrs) mag_out_bias


attrProxy :: (AttrWritable w, AttrNamed w, IvoryArea a, IvoryZero a)
          => w a
          -> ChanOutput a
          -> Tower e ()
attrProxy attr chan = do
  monitor (attrName attr ++ "Proxy") $ do
    handler chan "chan_write" $ do
      e <- attrEmitter attr
      callback $ \v -> emit e v

