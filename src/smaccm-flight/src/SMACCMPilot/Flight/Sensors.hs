{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors
  ( sensorTower
  ) where

import Data.Foldable
import Data.Maybe
import Data.Traversable
import Ivory.Language
import Ivory.Tower
import Linear
import Prelude hiding (foldr1, mapM)

import           Numeric.Estimator.Model.Pressure
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.BarometerSample as B
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample as G
import qualified SMACCMPilot.Comm.Ivory.Types.LidarliteSample as L
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult as R
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.RgbLedSetting as LED
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Flight.Sensors.GPS
import           SMACCMPilot.Hardware.SensorManager
import           SMACCMPilot.Hardware.Sensors
import           SMACCMPilot.INS.Bias.Gyro
import           SMACCMPilot.INS.Bias.Accel
import           SMACCMPilot.INS.Bias.Magnetometer.Tower
import           SMACCMPilot.INS.Bias.Calibration
import           SMACCMPilot.INS.DetectMotion
import           SMACCMPilot.INS.Ivory
import           SMACCMPilot.INS.SensorFusion
import           SMACCMPilot.INS.Tower
import           SMACCMPilot.Flight.Sensors.AccelBiasTrigger
import           SMACCMPilot.Flight.Sensors.LIDARLite

sensorTower :: (e -> FlightPlatform)
            -> ControllableVehicleAttrs Attr
            -> Tower e (Monitor e ())
sensorTower tofp attrs = do

  p   <- channel
  mon <- uartUbloxGPSTower tofp (fst p)
  attrProxy (gpsOutput attrs) (snd p)

  fp <- tofp <$> getEnv
  -- make a channel whether or not we have a LIDAR in order to make
  -- the control flow simpler here
  (lidar_in, lidar) <- channel
  let exti2cs = catMaybes [ mlidar ]
      mlidar = do
        LIDARLite{..} <- fp_lidarlite fp
        return $ ExternalSensor {
            ext_sens_name = "lidarlite"
          , ext_sens_init = \bpt init_chan ->
              lidarliteSensorManager
                bpt init_chan lidar_in lidarlite_i2c_addr
          }

  (a,g,m,b) <-
    sensorManager (fp_sensors . tofp) (fp_clockconfig . tofp) exti2cs

  motion <- channel
  detectMotion g a (fst motion)

  monitor "motion_light_debug" $ do
    handler (snd motion) "motion_light_debug" $ do
      e <- attrEmitter (rgbLed attrs)
      callbackV $ \mot -> do
        l <- local izero
        ifte_ mot
          (store (l ~> LED.red) 15)
          (store (l ~> LED.green) 15)
        emit e (constRef l)

  -- Need control law to ensure we only apply new calibrations when the system
  -- is disarmed.
  let cl_chan = attrReaderChan (controlLaw attrs)

  -- Accel: same basic calibration scheme as Gyro, below, except that we only
  -- trigger the calcAccelBiasTower under special conditions.
  attrProxy (accelRawOutput attrs) a

  accel_bias_trigger <- channel
  accelBiasTriggerTower (snd motion)
                        (attrReaderChan (px4ioState attrs))
                        (fst accel_bias_trigger)

  accel_bias <- calcAccelBiasTower a (snd accel_bias_trigger)
  attrProxy (accelCalibration attrs) accel_bias

  (accel_out, accel_out_bias) <- applyCalibrationTower accelCalibrate a accel_bias cl_chan

  attrProxy (accelOutputCalibration attrs) accel_out_bias
  attrProxy (accelOutput attrs) accel_out

  -- LIDAR: calibration handled onboard
  attrProxy (lidarliteOutput attrs) lidar

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

  gyro_bias <- calcGyroBiasTower g (snd motion)
  attrProxy (gyroCalibration attrs) gyro_bias

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


  -- Sensor fusion: estimate vehicle attitude with respect to a N/E/D
  -- navigation frame. Report (1) attitude; (2) sensor measurements in the
  -- navigation frame.
  states <- sensorFusion accel_out gyro_out mag_out (snd motion)
  monitor "sensor_fusion_proxy" $ do
    last_accel <- save "last_accel" accel_out
    last_baro <- save "last_baro" b
    last_gyro <- save "last_gyro" gyro_out
    last_lidar <- save "last_lidar" lidar

    handler states "new_state" $ do
      e <- attrEmitter $ sensorsOutput attrs
      callback $ \ stateVector -> do
        attitude <- mapM deref $ stateOrient $ stateVectorFromStruct stateVector
        let (Quaternion q0 (V3 q1 q2 q3)) = attitude

        accel <- xyzRef $ last_accel ~> A.sample

        baro_time <- deref $ last_baro ~> B.time
        pressure <- deref $ last_baro ~> B.pressure

        gyro_time <- deref $ last_gyro ~> G.time
        gyro <- xyzRef $ last_gyro ~> G.sample

        lidar_distance <- deref $ last_lidar ~> L.distance
        lidar_time <- deref $ last_lidar ~> L.time

        result <- local $ istruct
          [ R.valid .= ival (foldr1 (.||) $ fmap (/=? 0) attitude)
          , R.roll .= ival (atan2F (2 * (q0 * q1 + q2 * q3)) (1 - 2 * (q1 * q1 + q2 * q2)))
          , R.pitch .= ival (asin (2 * (q0 * q2 - q3 * q1)))
          , R.yaw .= ival (atan2F (2 * (q0 * q3 + q1 * q2)) (1 - 2 * (q2 * q2 + q3 * q3)))
          , R.omega .= xyzInitStruct (fmap (* (pi / 180)) gyro)
          , R.baro_alt .= ival (pressureToHeight $ pressure * 100) -- convert mbar to Pascals
          , R.lidar_alt .= ival lidar_distance
          , R.accel .= xyzInitStruct accel
          , R.ahrs_time .= ival gyro_time
          , R.baro_time .= ival baro_time
          , R.lidar_time .= ival lidar_time
          ]
        emit e $ constRef result
  return mon

save :: (IvoryArea a, IvoryZero a)
     => String
     -> ChanOutput a
     -> Monitor e (ConstRef 'Global a)
save name src = do
  copy <- state name
  handler src ("save_" ++ name) $ do
    callback $ refCopy copy
  return $ constRef copy

xyzRef :: ConstRef s ('Struct "xyz") -> Ivory eff (V3 IFloat)
xyzRef r = mapM deref $ fmap (r ~>) (V3 XYZ.x XYZ.y XYZ.z)

xyzInitStruct :: V3 IFloat -> Init ('Struct "xyz")
xyzInitStruct (V3 x y z) = istruct [ XYZ.x .= ival x, XYZ.y .= ival y, XYZ.z .= ival z ]
