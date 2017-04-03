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
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample as M
import qualified SMACCMPilot.Comm.Ivory.Types.Quaternion as Q
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult as R
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.RgbLedSetting as LED
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import qualified SMACCMPilot.Flight.Control.Attitude.KalmanFilter as Att
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Flight.Sensors.GPS
import           SMACCMPilot.Hardware.HMC5883L
import           SMACCMPilot.Hardware.SensorManager
import           SMACCMPilot.Hardware.Sensors
import           SMACCMPilot.Flight.Sensors.DetectMotion
import           SMACCMPilot.Flight.Sensors.LIDARLite
import           SMACCMPilot.Flight.Sensors.PX4Flow

sensorTower :: (e -> FlightPlatform)
            -> ControllableVehicleAttrs Attr
            -> Tower e (Monitor e ())
sensorTower tofp attrs = do

  gps <- channel
  mon <- uartUbloxGPSTower tofp (fst gps)
  attrProxy (gpsOutput attrs) (snd gps)

  fp <- tofp <$> getEnv
  -- make channels whether or not we have a LIDAR or PX4Flow in order
  -- to make the control flow simpler here
  (lidar_in, lidar) <- channel
  (px4flow_in, px4flow) <- channel
  (hmc5883l_in, hmc5883l) <- channel
  let exti2cs = catMaybes [ mlidar, mpx4flow, mhmc5883l ]
      mlidar = do
        ll@LIDARLite{..} <- fp_lidarlite fp
        return $ ExternalSensor {
            ext_sens_name = "lidarlite"
          , ext_sens_init = \bpt init_chan ->
              lidarliteSensorManager
                bpt init_chan lidar_in ll
          }
      mpx4flow = do
        p4f@PX4Flow{..} <- fp_px4flow fp
        return $ ExternalSensor {
            ext_sens_name = "px4flow"
          , ext_sens_init = \bpt init_chan ->
              px4flowSensorManager
                bpt init_chan px4flow_in p4f
          }
      mhmc5883l = do
        HMC5883L{..} <- fp_ext_hmc5883l fp
        return $ ExternalSensor {
            ext_sens_name = "hmc5883l"
          , ext_sens_init = \bpt init_chan ->
              hmc5883lSensorManager hmc5883l_mag_cal
                bpt init_chan hmc5883l_in hmc5883l_i2c_addr
          }

  (accel, gyro_raw, internal_mag_raw, baro_raw) <-
    sensorManager (fp_sensors . tofp) (fp_clockconfig . tofp) exti2cs

  let mag_raw = case mhmc5883l of
                  Nothing -> internal_mag_raw
                  Just _ -> hmc5883l

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

  attrProxy (accelOutput attrs) accel

  -- LIDAR: calibration handled onboard
  attrProxy (lidarliteOutput attrs) lidar

  attrProxy (px4flowOutput attrs) px4flow

  -- Baro: no calibration at this time.
  attrProxy (baroOutput attrs) baro_raw

  attrProxy (gyroOutput attrs) gyro_raw

  attrProxy (magOutput attrs) mag_raw

  -- Sensor fusion: estimate vehicle attitude with respect to a N/E/D
  -- navigation frame. Report (1) attitude; (2) sensor measurements in the
  -- navigation frame.
  states <- Att.sensorFusion
              (sensors_local_mag (fp_sensors fp))
              accel gyro_raw mag_raw (snd motion)
  monitor "sensor_fusion_proxy" $ do
    last_accel <- save "last_accel" accel
    last_mag <- save "last_mag" mag_raw
    last_baro <- save "last_baro" baro_raw
    last_gyro <- save "last_gyro" gyro_raw
    last_lidar <- save "last_lidar" lidar

    handler states "new_state" $ do
      e <- attrEmitter $ sensorsOutput attrs
      callback $ \ ahrsState -> do
        Att.AttState {..} <- Att.attState ahrsState
        let (Quaternion q0 (V3 q1 q2 q3)) = ahrs_ltp_to_body

        accel_sample <- xyzRef $ last_accel ~> A.sample
        accel_valid <- iNot <$> deref (last_accel ~> A.samplefail)

        mag_valid <- iNot <$> deref (last_mag ~> M.samplefail)

        baro_time <- deref $ last_baro ~> B.time
        pressure <- deref $ last_baro ~> B.pressure
        baro_valid <- iNot <$> deref (last_baro ~> B.samplefail)

        gyro_time <- deref $ last_gyro ~> G.time
        gyro_valid <- iNot <$> deref (last_gyro ~> G.samplefail)

        lidar_distance <- deref $ last_lidar ~> L.distance
        lidar_time <- deref $ last_lidar ~> L.time
        lidar_valid <- iNot <$> deref (last_lidar ~> L.samplefail)

        samples_valid <- assign $ accel_valid
                              .&& mag_valid
                              .&& baro_valid
                              .&& gyro_valid
                              .&& (maybe true (const lidar_valid) mlidar)

        att_quat_valid <- assign $ foldr1 (.||) $ fmap (/=? 0) ahrs_ltp_to_body

        result <- local $ istruct
          [ R.valid .= ival (samples_valid .&& att_quat_valid)
          , R.roll .= ival (atan2F (2 * (q0 * q1 + q2 * q3)) (1 - 2 * (q1 * q1 + q2 * q2)))
          , R.pitch .= ival (asin (2 * (q0 * q2 - q3 * q1)))
          , R.yaw .= ival (atan2F (2 * (q0 * q3 + q1 * q2)) (1 - 2 * (q2 * q2 + q3 * q3)))
          , R.omega .= xyzInitStruct ahrs_body_rates
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
