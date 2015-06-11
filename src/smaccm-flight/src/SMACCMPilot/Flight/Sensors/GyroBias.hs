{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors.GyroBias
  ( gyroBiasTower
  , gyroBiasTower'
  ) where

import Ivory.Language
import Ivory.Tower
import SMACCMPilot.INS.Bias.Gyro
import SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ

gyroBiasTower :: ChanOutput (Struct "gyroscope_sample")
              -> ChanOutput (Struct "accelerometer_sample")
              -> Tower e (ChanOutput (Struct "xyz_calibration"))
gyroBiasTower g a = do
  cal <- channel
  p <- period (Milliseconds 1000)
  gyroBiasTower' g a (fst cal) p
  return (snd cal)

gyroBiasTower' :: ChanOutput (Struct "gyroscope_sample")
               -> ChanOutput (Struct "accelerometer_sample")
               -> ChanInput  (Struct "xyz_calibration")
               -> ChanOutput (Stored ITime)
               -> Tower e ()
gyroBiasTower' g a c newoutput = monitor "gyroBias" $ do
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
    , C.time     .= ival (timeMicrosFromITime time)
    ]

