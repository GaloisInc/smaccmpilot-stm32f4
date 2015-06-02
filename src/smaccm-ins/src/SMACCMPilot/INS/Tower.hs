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
import Prelude hiding (mapM)
import qualified SMACCMPilot.Hardware.GPS.Types as GPS
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.BarometerSample     as B
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz as XYZ
import SMACCMPilot.INS.Ivory
import SMACCMPilot.INS.SensorFusion
import SMACCMPilot.Time

changeUnits :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)
changeUnits f = fmap (fmap f)

accel :: (SafeCast IFloat to)
      => ConstRef s (Struct "accelerometer_sample")
      -> Ivory eff (XYZ to)
accel sample = fmap (fmap safeCast)
             $ mapM deref
             $ fmap ((sample ~> A.sample) ~>)
             $ xyz XYZ.x XYZ.y XYZ.z

gyro :: (Floating to, SafeCast IFloat to)
      => ConstRef s (Struct "gyroscope_sample")
      -> Ivory eff (XYZ to)
gyro sample = changeUnits (* (pi / 180))
            $ fmap (fmap safeCast)
            $ mapM deref
            $ fmap ((sample ~> G.sample) ~>)
            $ xyz XYZ.x XYZ.y XYZ.z

kalman_predict :: Def ('[ Ref s1 (Struct "kalman_state")
                        , Ref s2 (Struct "kalman_covariance")
                        , Ref s3 (Stored ITime)
                        , ITime
                        , Ref s4 (Struct "accelerometer_sample")
                        , Ref s5 (Struct "gyroscope_sample")] :-> ())
kalman_predict = proc "kalman_predict" $
  \ state_vector covariance last_predict now last_accel last_gyro -> body $ do
      acc_not_ready <- deref $ last_accel ~> A.samplefail
      gyro_not_ready <- deref $ last_gyro ~> G.samplefail
      when (acc_not_ready .|| gyro_not_ready) retVoid

      last_time <- deref last_predict
      let dt = safeCast (castDefault (toIMicroseconds (now - last_time)) :: Sint32) * 1.0e-6
      distVector <- DisturbanceVector
        <$> gyro (constRef last_gyro)
        <*> accel (constRef last_accel)
      kalmanPredict state_vector covariance dt distVector
      store last_predict now

      reset_acc  <- local $ istruct [ A.samplefail .= ival true ]
      refCopy last_accel reset_acc
      reset_gyro <- local $ istruct [ G.samplefail .= ival true ]
      refCopy last_gyro reset_gyro

mag :: (Num to, SafeCast IFloat to)
    => ConstRef s (Struct "magnetometer_sample")
    -> Ivory eff (XYZ to)
mag sample = changeUnits (* 1000)
           $ fmap (fmap safeCast)
           $ mapM deref
           $ fmap ((sample ~> M.sample) ~>)
           $ xyz XYZ.x XYZ.y XYZ.z

mag_measure :: Def ('[ Ref s1 (Struct "kalman_state")
                     , Ref s2 (Struct "kalman_covariance")
                     , ConstRef s3 (Struct "magnetometer_sample")] :-> ())
mag_measure = proc "mag_measure" $ \ state_vector covariance last_mag -> body $ do
  magMeasure state_vector covariance =<< mag last_mag

init_filter :: Def ('[ Ref s1 (Struct "kalman_state")
                     , Ref s2 (Struct "kalman_covariance")
                     , ConstRef s3 (Struct "accelerometer_sample")
                     , ConstRef s4 (Struct "magnetometer_sample")
                     ] :-> IBool)
init_filter = proc "init_filter" $
  \ state_vector covariance last_accel last_mag -> body $ do
      magFail <- deref $ last_mag ~> M.samplefail
      when (iNot magFail) $ do
        acc <- accel last_accel
        mag' <- mag last_mag
        kalmanInit state_vector covariance acc mag'
        ret true
      ret false

storeSum :: (Num a, IvoryStore a) => Ref s1 (Stored a) -> ConstRef s2 (Stored a) -> Ivory eff ()
storeSum dst src = do
  old <- deref dst
  new <- deref src
  store dst $ old + new

sensorFusion :: ChanOutput (Struct "accelerometer_sample")
             -> ChanOutput (Struct "gyroscope_sample")
             -> ChanOutput (Struct "magnetometer_sample")
             -> ChanOutput (Struct "barometer_sample")
             -> ChanOutput (Struct "position")
             -> Tower e (ChanOutput (Struct "kalman_state"))
sensorFusion accelSource gyroSource magSource _baroSource _gpsSource = do
  (stateSink, stateSource) <- channel

  mapM_ towerDepends
    [ GPS.gpsTypesModule
    , A.accelerometerSampleTypesModule
    , G.gyroscopeSampleTypesModule
    , M.magnetometerSampleTypesModule
    , B.barometerSampleTypesModule
    ]

  towerDepends insTypesModule
  towerModule  insTypesModule

  monitor "fuse" $ do
    monitorModuleDef $ do
      incl kalman_predict
      incl mag_measure
      incl init_filter

    initialized <- state "initialized"
    last_predict <- state "last_predict"
    state_vector <- state "state_vector"
    covariance <- state "covariance"

    last_gyro <- stateInit "last_gyro" $ istruct [ G.samplefail .= ival true ]
    last_acc  <- stateInit "last_acc" $ istruct [ A.samplefail .= ival true ]
    last_mag <- stateInit "last_mag" $ istruct [ M.samplefail .= ival true ]

    handler accelSource "accel" $ do
      stateEmit <- emitter stateSink 1
      callback $ \ sample -> do
        accelFail <- deref $ sample ~> A.samplefail
        unless accelFail $ do
          store (last_acc ~> A.samplefail) false
          xyzMap $ \ i -> storeSum (last_acc ~> A.sample ~> i) (sample ~> A.sample ~> i)
          refCopy (last_acc ~> A.time) (sample ~> A.time)

          ready <- deref initialized
          ifte_ ready
            (do
              now <- fmap iTimeFromTimeMicros $ deref $ last_acc ~> A.time
              call_ kalman_predict state_vector covariance last_predict now last_acc last_gyro
              emit stateEmit $ constRef state_vector
            ) (do
              done <- call init_filter state_vector
                                       covariance
                                       (constRef last_acc)
                                       (constRef last_mag)
              when done $ do
                store initialized true
                t <- deref $ last_acc ~> A.time
                store last_predict (iTimeFromTimeMicros t)
            )

    handler gyroSource "gyro" $ do
      stateEmit <- emitter stateSink 1
      callback $ \ sample -> do
        gyroFail <- deref $ sample ~> G.samplefail
        unless gyroFail $ do
          store (last_gyro ~> G.samplefail) false
          xyzMap $ \ i -> storeSum (last_gyro ~> G.sample ~> i) (sample ~> G.sample ~> i)
          refCopy (last_gyro ~> G.time) (sample ~> G.time)

          ready <- deref initialized
          when ready $ do
            now <- fmap iTimeFromTimeMicros $ deref $ last_gyro ~> G.time
            call_ kalman_predict state_vector covariance last_predict now last_acc last_gyro
            emit stateEmit $ constRef state_vector

    handler magSource "mag" $ callback $ \ sample -> do
      failed <- deref $ sample ~> M.samplefail
      unless failed $ do
        refCopy last_mag sample
        ready <- deref initialized
        when ready $ call_ mag_measure state_vector covariance $ constRef last_mag

  return stateSource

  where
  xyzMap :: (Label "xyz" (Stored IFloat) -> Ivory eff ()) -> Ivory eff ()
  xyzMap f = do
    f XYZ.x
    f XYZ.y
    f XYZ.z

