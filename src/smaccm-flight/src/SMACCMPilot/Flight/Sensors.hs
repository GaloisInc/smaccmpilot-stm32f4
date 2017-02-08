{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors
  ( sensorTower
  ) where

import Prelude ()
import Prelude.Compat

import Data.Maybe
import Ivory.Language
import Ivory.Tower
import Linear
import Prelude hiding (foldr1, mapM)

import           Numeric.Estimator.Model.Pressure
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.BarometerSample as B
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample as G
import qualified SMACCMPilot.Comm.Ivory.Types.LidarliteSample as L
import qualified SMACCMPilot.Comm.Ivory.Types.Quaternion as Q
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
import           SMACCMPilot.INS.Bias.Magnetometer.Tower
import           SMACCMPilot.INS.Bias.Calibration
import           SMACCMPilot.INS.DetectMotion
import           SMACCMPilot.INS.Attitude.Ivory
import           SMACCMPilot.INS.Attitude.SensorFusion
import           SMACCMPilot.INS.Attitude.Tower
import           SMACCMPilot.Flight.Sensors.LIDARLite
import           SMACCMPilot.Flight.Sensors.PX4Flow

sensorTower :: (e -> FlightPlatform)
            -> ControllableVehicleAttrs Attr
            -> Tower e (Monitor e ())
sensorTower tofp attrs = do

  p   <- channel
  mon <- uartUbloxGPSTower tofp (fst p)
  attrProxy (gpsOutput attrs) (snd p)

  fp <- tofp <$> getEnv
  -- make channels whether or not we have a LIDAR or PX4Flow in order
  -- to make the control flow simpler here
  (lidar_in, lidar) <- channel
  (px4flow_in, px4flow) <- channel
  let exti2cs = catMaybes [ mlidar, mpx4flow ]
      mlidar = do
        ll@LIDARLite{..} <- fp_lidarlite fp
        return $ ExternalSensor {
            ext_sens_name = "lidarlite"
          , ext_sens_init = \bpt init_chan ->
              lidarliteSensorManager
                bpt init_chan lidar_in ll
          }
      mpx4flow = do
        PX4Flow{..} <- fp_px4flow fp
        return $ ExternalSensor {
            ext_sens_name = "px4flow"
          , ext_sens_init = \bpt init_chan ->
              px4flowSensorManager
                bpt init_chan px4flow_in px4flow_i2c_addr
          }

  (accel, gyro_raw, mag_raw, baro_raw) <-
    sensorManager (fp_sensors . tofp) (fp_clockconfig . tofp) exti2cs

  motion <- channel
  detectMotion gyro_raw accel (fst motion)

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

  attrProxy (accelOutput attrs) accel

  -- LIDAR: calibration handled onboard
  attrProxy (lidarliteOutput attrs) lidar

  attrProxy (px4flowOutput attrs) px4flow

  -- Baro: no calibration at this time.
  attrProxy (baroOutput attrs) baro_raw

  -- Gyro: there are four attributes we care about.
  --    gyroRawOutput: the uncalibrated gyro sample, from sensorManager
  --    gyroCalibration: the most recent calibration value calculated, from
  --      calcGyroBias.
  --    gyroOutputCalibration: the calibration value, selected by applyCal
  --      applyCal will only choose a new calibration value when the
  --      control law indicated the vehicle is disarmed.
  --    gyroOutput: the calibrated gyro sample, from applyGyroBias

  attrProxy (gyroRawOutput attrs) gyro_raw

  gyro_bias <- calcGyroBiasTower gyro_raw (snd motion)
  attrProxy (gyroCalibration attrs) gyro_bias

  (gyro_out, gyro_out_bias) <- applyCalibrationTower gyroCalibrate gyro_raw gyro_bias cl_chan
  attrProxy (gyroOutput            attrs) gyro_out
  attrProxy (gyroOutputCalibration attrs) gyro_out_bias

  -- Mag: same basic idea as Gyro. We calculate calibration differently, of
  -- course.
  attrProxy (magRawOutput attrs) mag_raw

  mag_bias <- calcMagBiasTower mag_raw
  attrProxy (magCalibration attrs) mag_bias

  (mag_out, mag_out_bias) <- applyCalibrationTower magCalibrate mag_raw mag_bias cl_chan
  attrProxy (magOutput            attrs) mag_out
  attrProxy (magOutputCalibration attrs) mag_out_bias

  -- Sensor fusion: estimate vehicle attitude with respect to a N/E/D
  -- navigation frame. Report (1) attitude; (2) sensor measurements in the
  -- navigation frame.
  states <- sensorFusion accel gyro_out mag_out (snd motion)
  monitor "sensor_fusion_proxy" $ do
    last_accel <- save "last_accel" accel
    last_baro <- save "last_baro" baro_raw
    last_gyro <- save "last_gyro" gyro_out
    last_lidar <- save "last_lidar" lidar

    handler states "new_state" $ do
      e <- attrEmitter $ sensorsOutput attrs
      callback $ \ stateVector -> do
        attitude <- mapM deref $ stateOrient $ stateVectorFromStruct stateVector
        let (Quaternion q0 (V3 q1 q2 q3)) = attitude

        accel_sample <- xyzRef $ last_accel ~> A.sample

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
          , R.attitude .= quatInitStruct q0 q1 q2 q3
          , R.baro_alt .= ival (pressureToHeight $ pressure * 100) -- convert mbar to Pascals
          , R.lidar_alt .= ival lidar_distance
          , R.accel .= xyzInitStruct accel_sample
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

quatInitStruct
  :: IFloat -> IFloat -> IFloat -> IFloat -> Init ('Struct "quaternion")
quatInitStruct a b c d =
  istruct [ Q.quat_a .= ival a
          , Q.quat_b .= ival b
          , Q.quat_c .= ival c
          , Q.quat_d .= ival d
          ]
