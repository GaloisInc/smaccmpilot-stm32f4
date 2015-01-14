{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.INS.Tower (sensorFusion) where

import Control.Applicative
import Data.Traversable
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion
import Prelude hiding (mapM)
import qualified SMACCMPilot.Hardware.GPS.Types as GPS
import qualified SMACCMPilot.Hardware.HMC5883L.Types as HMC5883L
import qualified SMACCMPilot.Hardware.MPU6000.Types as MPU6000
import qualified SMACCMPilot.Hardware.MS5611.Types as MS5611
import SMACCMPilot.INS.Ivory

accel :: SafeCast IFloat to => Ref s (Struct "mpu6000_sample") -> Ivory eff (XYZ to)
accel sample = fmap (fmap safeCast) $ mapM deref $ fmap (sample ~>) $ xyz MPU6000.accel_x MPU6000.accel_y MPU6000.accel_z

gyro :: SafeCast IFloat to => Ref s (Struct "mpu6000_sample") -> Ivory eff (XYZ to)
gyro sample = fmap (fmap safeCast) $ mapM deref $ fmap (sample ~>) $ xyz MPU6000.gyro_x MPU6000.gyro_y MPU6000.gyro_z

mag :: SafeCast IFloat to => Ref s (Struct "hmc5883l_sample") -> Ivory eff (XYZ to)
mag sample = fmap (fmap safeCast) $ mapM deref $ fmap ((sample ~> HMC5883L.sample) !) $ xyz 0 1 2

pressure :: SafeCast IFloat to => Ref s (Struct "ms5611_measurement") -> Ivory eff to
pressure sample = fmap safeCast $ deref $ sample ~> MS5611.pressure

sensorFusion :: ChanOutput (Struct "mpu6000_sample")
             -> ChanOutput (Struct "hmc5883l_sample")
             -> ChanOutput (Struct "ms5611_measurement")
             -> ChanOutput (Struct "position")
             -> Tower e (ChanOutput (Struct "kalman_state"))
sensorFusion gyroSource magSource baroSource _gpsSource = do
  (stateSink, stateSource) <- channel

  mapM_ towerDepends
    [ GPS.gpsTypesModule
    , HMC5883L.hmc5883lTypesModule
    , MPU6000.mpu6000TypesModule
    , MS5611.ms5611TypesModule
    ]

  monitor "fuse" $ do
    initialized <- state "initialized"
    last_predict <- state "last_predict"
    state_vector <- state "state_vector"
    covariance <- state "covariance"

    last_gyro <- stateInit "last_gyro" $ istruct [ MPU6000.samplefail .= ival true ]
    last_mag <- stateInit "last_mag" $ istruct [ HMC5883L.samplefail .= ival true ]
    last_baro <- stateInit "last_baro" $ istruct [ MS5611.sampfail .= ival true ]

    handler gyroSource "gyro" $ do
      stateEmit <- emitter stateSink 1
      callback $ \ sample -> do
        gyroFail <- deref $ sample ~> MPU6000.samplefail
        unless gyroFail $ do
          refCopy last_gyro sample

          now <- deref $ last_gyro ~> MPU6000.time
          acc <- accel last_gyro

          ready <- deref initialized
          ifte_ ready
            (do
              distVector <- DisturbanceVector <$> gyro last_gyro <*> pure acc
              last_time <- deref last_predict
              let dt = safeCast (toIMicroseconds (now - last_time)) * 1.0e-6
              kalmanPredict state_vector covariance dt distVector
              store last_predict now
              emit stateEmit $ constRef state_vector
            ) (do
              magFail <- deref $ last_mag ~> HMC5883L.samplefail
              baroFail <- deref $ last_baro ~> MS5611.sampfail
              when (iNot magFail .&& iNot baroFail) $ do
                mag' <- mag last_mag
                kalmanInit state_vector covariance acc mag' =<< pressure last_baro
                store initialized true
                store last_predict now
            )

    handler magSource "mag" $ callback $ \ sample -> do
      failed <- deref $ sample ~> HMC5883L.samplefail
      unless failed $ do
        refCopy last_mag sample
        ready <- deref initialized
        when ready $ magMeasure state_vector covariance =<< mag last_mag

    handler baroSource "baro" $ callback $ \ sample -> do
      failed <- deref $ sample ~> MS5611.sampfail
      unless failed $ do
        refCopy last_baro sample
        ready <- deref initialized
        when ready $ pressureMeasure state_vector covariance =<< pressure last_baro

  return stateSource
