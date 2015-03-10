{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

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
import qualified SMACCMPilot.Hardware.Types.Magnetometer  as M
import qualified SMACCMPilot.Hardware.Types.Accelerometer as A
import qualified SMACCMPilot.Hardware.Types.Gyroscope     as G
import qualified SMACCMPilot.Hardware.Types.Barometer     as B
import SMACCMPilot.INS.Ivory

changeUnits :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)
changeUnits f = fmap (fmap f)

accel :: (SafeCast IFloat to)
      => ConstRef s (Struct "accelerometer_sample")
      -> Ivory eff (XYZ to)
accel sample = fmap (fmap safeCast)
             $ mapM deref
             $ fmap ((sample ~> A.sample) !)
             $ xyz 0 1 2

gyro :: (Floating to, SafeCast IFloat to)
      => ConstRef s (Struct "gyroscope_sample")
      -> Ivory eff (XYZ to)
gyro sample = changeUnits (* (pi / 180))
            $ fmap (fmap safeCast)
            $ mapM deref
            $ fmap ((sample ~> G.sample) !)
            $ xyz 0 1 2

kalman_predict :: Def ('[ Ref s1 (Struct "kalman_state")
                        , Ref s2 (Struct "kalman_covariance")
                        , IFloat
                        , ConstRef s3 (Struct "accelerometer_sample")
                        , ConstRef s4 (Struct "gyroscope_sample")] :-> ())
kalman_predict = proc "kalman_predict" $
  \ state_vector covariance dt last_accel last_gyro -> body $ do
      distVector <- DisturbanceVector <$> gyro last_gyro <*> accel last_accel
      kalmanPredict state_vector covariance dt distVector

mag :: (Num to, SafeCast IFloat to)
    => ConstRef s (Struct "magnetometer_sample")
    -> Ivory eff (XYZ to)
mag sample = changeUnits (* 1000)
           $ fmap (fmap safeCast)
           $ mapM deref
           $ fmap ((sample ~> M.sample) !)
           $ xyz 0 1 2

mag_measure :: Def ('[ Ref s1 (Struct "kalman_state")
                     , Ref s2 (Struct "kalman_covariance")
                     , ConstRef s3 (Struct "magnetometer_sample")] :-> ())
mag_measure = proc "mag_measure" $ \ state_vector covariance last_mag -> body $ do
  magMeasure state_vector covariance =<< mag last_mag

pressure :: (Num to, SafeCast IFloat to)
         => ConstRef s (Struct "barometer_sample")
         -> Ivory eff to
pressure sample = fmap (* 100)
                $ fmap safeCast
                $ deref
                $ sample ~> B.pressure

pressure_measure :: Def ('[ Ref s1 (Struct "kalman_state")
                          , Ref s2 (Struct "kalman_covariance")
                          , ConstRef s3 (Struct "barometer_sample")] :-> ())
pressure_measure = proc "pressure_measure" $
  \ state_vector covariance last_baro -> body $ do
      pressureMeasure state_vector covariance =<< pressure last_baro

init_filter :: Def ('[ Ref s1 (Struct "kalman_state")
                     , Ref s2 (Struct "kalman_covariance")
                     , ConstRef s3 (Struct "accelerometer_sample")
                     , ConstRef s4 (Struct "gyroscope_sample")
                     , ConstRef s5 (Struct "magnetometer_sample")
                     , ConstRef s6 (Struct "barometer_sample")] :-> IBool)
init_filter = proc "init_filter" $
  \ state_vector covariance last_accel last_gyro last_mag last_baro -> body $ do
      magFail <- deref $ last_mag ~> M.samplefail
      baroFail <- deref $ last_baro ~> B.sampfail
      when (iNot magFail .&& iNot baroFail) $ do
        acc <- accel last_accel
        mag' <- mag last_mag
        -- XXX do we need gyro for init? if not i guess we can remove it from
        -- these arguments.
        kalmanInit state_vector covariance acc mag' =<< pressure last_baro
        ret true
      ret false

sensorFusion :: ChanOutput (Struct "accelerometer_sample")
             -> ChanOutput (Struct "gyroscope_sample")
             -> ChanOutput (Struct "magnetometer_sample")
             -> ChanOutput (Struct "barometer_sample")
             -> ChanOutput (Struct "position")
             -> Tower e (ChanOutput (Struct "kalman_state"))
sensorFusion accelSource gyroSource magSource baroSource _gpsSource = do
  (stateSink, stateSource) <- channel

  mapM_ towerDepends
    [ GPS.gpsTypesModule
    , A.accelerometerTypesModule
    , G.gyroscopeTypesModule
    , M.magnetometerTypesModule
    , B.barometerTypesModule
    ]

  towerDepends insTypesModule
  towerModule  insTypesModule

  monitor "fuse" $ do
    monitorModuleDef $ do
      incl kalman_predict
      incl mag_measure
      incl pressure_measure
      incl init_filter

    initialized <- state "initialized"
    last_predict <- state "last_predict"
    state_vector <- state "state_vector"
    covariance <- state "covariance"

    last_gyro <- stateInit "last_gyro" $ istruct [ G.samplefail .= ival true ]
    last_acc  <- stateInit "last_acc" $ istruct [ A.samplefail .= ival true ]
    last_mag <- stateInit "last_mag" $ istruct [ M.samplefail .= ival true ]
    last_baro <- stateInit "last_baro" $ istruct [ B.sampfail .= ival true ]

    handler accelSource "accel" $ do
      callback $ \sample -> do
        accelFail <- deref $ sample ~> A.samplefail
        unless accelFail $ do
          refCopy last_acc sample
      -- XXX if we're depending on accel for init, but not gyro,
      -- should we move the init logic to inside the accel handler?
      -- We aren't guaranteed anything about the order of delivery
      -- of accel and gyro - they're no longer logically connected

    handler gyroSource "gyro" $ do
      stateEmit <- emitter stateSink 1
      callback $ \ sample -> do
        gyroFail <- deref $ sample ~> G.samplefail
        unless gyroFail $ do
          refCopy last_gyro sample

          ready <- deref initialized
          ifte_ ready
            (do
              last_time <- deref last_predict
              now <- deref $ last_gyro ~> G.time
              let dt = safeCast (castDefault (toIMicroseconds (now - last_time)) :: Sint32) * 1.0e-6
              call_ kalman_predict state_vector covariance dt (constRef last_acc) (constRef last_gyro)
              store last_predict now
              emit stateEmit $ constRef state_vector
            ) (do
              done <- call init_filter state_vector
                                       covariance
                                       (constRef last_acc)
                                       (constRef last_gyro)
                                       (constRef last_mag)
                                       (constRef last_baro)
              when done $ do
                store initialized true
                refCopy last_predict $ last_gyro ~> G.time
            )

    handler magSource "mag" $ callback $ \ sample -> do
      failed <- deref $ sample ~> M.samplefail
      unless failed $ do
        refCopy last_mag sample
        ready <- deref initialized
        when ready $ call_ mag_measure state_vector covariance $ constRef last_mag

    handler baroSource "baro" $ callback $ \ sample -> do
      failed <- deref $ sample ~> B.sampfail
      unless failed $ do
        refCopy last_baro sample
        ready <- deref initialized
        when ready $ call_ pressure_measure state_vector covariance $ constRef last_baro

  return stateSource
