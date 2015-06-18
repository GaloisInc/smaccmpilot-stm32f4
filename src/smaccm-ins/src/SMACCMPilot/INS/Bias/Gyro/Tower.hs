{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Bias.Gyro.Tower
  ( calcGyroBiasTower
  , calcGyroBiasTower'
  , gyroCalibrate
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Bias.Gyro.Estimator
import SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ

calcGyroBiasTower :: ChanOutput (Struct "gyroscope_sample")
                  -> ChanOutput (Struct "accelerometer_sample")
                  -> Tower e (ChanOutput (Struct "xyz_calibration"))
calcGyroBiasTower g a = do
  cal <- channel
  p <- period (Milliseconds 1000)
  calcGyroBiasTower' g a (fst cal) p
  return (snd cal)

calcGyroBiasTower' :: ChanOutput (Struct "gyroscope_sample")
                   -> ChanOutput (Struct "accelerometer_sample")
                   -> ChanInput  (Struct "xyz_calibration")
                   -> ChanOutput (Stored ITime)
                   -> Tower e ()
calcGyroBiasTower' g a c newoutput = monitor "calcGyroBias" $ do
  gbe <- monitorGyroBiasEstimator
  handler systemInit "init" $ callback $ const $ do
    gbe_init gbe

  handler g "gyro" $ callback $ \g_samp -> do
    g_axes <- mkGyroSample g_samp
    gbe_gyro_sample gbe g_axes

  handler a "accel" $ callback $ \a_samp -> do
    a_axes <- mkAccelSample a_samp
    gbe_accel_sample gbe a_axes

  handler newoutput "output" $ do
    e <- emitter c 1
    callbackV $ \t -> do
      biases <- local izero
      done <- gbe_output gbe biases
      cal <- mkCalibration (constRef biases) done t
      emit e cal

mkGyroSample :: (GetAlloc eff ~ Scope s)
             => ConstRef s' (Struct "gyroscope_sample")
             -> Ivory eff (ConstRef (Stack s) (Array 3 (Stored IFloat)))
mkGyroSample sample = do
  x <- deref ((sample ~> G.sample) ~> XYZ.x)
  y <- deref ((sample ~> G.sample) ~> XYZ.y)
  z <- deref ((sample ~> G.sample) ~> XYZ.z)
  fmap constRef $ local $ iarray [ ival x, ival y, ival z ]

mkAccelSample :: (GetAlloc eff ~ Scope s)
              => ConstRef s' (Struct "accelerometer_sample")
              -> Ivory eff (ConstRef (Stack s) (Array 3 (Stored IFloat)))
mkAccelSample sample = do
  x <- deref ((sample ~> A.sample) ~> XYZ.x)
  y <- deref ((sample ~> A.sample) ~> XYZ.y)
  z <- deref ((sample ~> A.sample) ~> XYZ.z)
  fmap constRef $ local $ iarray [ ival x, ival y, ival z ]

mkCalibration :: (GetAlloc eff ~ Scope s)
              => ConstRef s' (Array 3 (Stored IFloat))
              -> IBool
              -> ITime
              -> Ivory eff (ConstRef (Stack s) (Struct "xyz_calibration"))
mkCalibration cal done time = do
  x <- deref (cal ! 0)
  y <- deref (cal ! 1)
  z <- deref (cal ! 2)
  fmap constRef $ local $ istruct
    [ C.valid    .= ival done
    , C.progress .= ival (done ? (1.0, 0.0))
    , C.bias     .= istruct
        [ XYZ.x  .= ival x
        , XYZ.y  .= ival y
        , XYZ.z  .= ival z
        ]
    , C.scale    .= istruct
        [ XYZ.x  .= ival 1
        , XYZ.y  .= ival 1
        , XYZ.z  .= ival 1
        ]
    , C.time     .= ival (timeMicrosFromITime time)
    ]

gyroCalibrate :: Calibrate (Struct "gyroscope_sample")
gyroCalibrate = Calibrate aux
  where
  aux samp cal = do
    out <- local izero
    refCopy out samp
    v <- deref (cal ~> C.valid)
    when v $ do
      bx <- deref ((cal ~> C.bias) ~> XYZ.x)
      by <- deref ((cal ~> C.bias) ~> XYZ.y)
      bz <- deref ((cal ~> C.bias) ~> XYZ.z)
      ((out ~> G.sample) ~> XYZ.x) %= subtract bx
      ((out ~> G.sample) ~> XYZ.y) %= subtract by
      ((out ~> G.sample) ~> XYZ.z) %= subtract bz
      store (out ~> G.calibrated) true
    return (constRef out)
