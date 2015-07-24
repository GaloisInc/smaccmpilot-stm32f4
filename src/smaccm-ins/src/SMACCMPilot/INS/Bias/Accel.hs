{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.INS.Bias.Accel
  ( calcAccelBiasTower
  , calcAccelBiasTower'
  , accelCalibrate
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Filter
import SMACCMPilot.Time

calcAccelBiasTower :: ChanOutput (Struct "accelerometer_sample")
                   -> ChanOutput (Stored IBool)
                   -> Tower e (ChanOutput (Struct "xyz_calibration"))
calcAccelBiasTower a motion = do
  cal <- channel
  p <- period (Milliseconds 1000)
  calcAccelBiasTower' a motion (fst cal) p
  return (snd cal)

calcAccelBiasTower' :: ChanOutput (Struct "accelerometer_sample")
                    -> ChanOutput (Stored IBool)
                    -> ChanInput  (Struct "xyz_calibration")
                    -> ChanOutput (Stored ITime)
                    -> Tower e ()
calcAccelBiasTower' a motion c newoutput = monitor "calcAccelBias" $ do
  n <- freshname "abe"
  let named s = showUnique n ++ "_" ++ s

  let lowpass fn = ivoryRunningAverageFilter fn (Proxy :: Proxy 64)

  let (lpf_ax, lpf_ax_moddef) = lowpass (named "ax")
  let (lpf_ay, lpf_ay_moddef) = lowpass (named "ay")
  let (lpf_az, lpf_az_moddef) = lowpass (named "az")

  monitorModuleDef $ do
    lpf_ax_moddef
    lpf_ay_moddef
    lpf_az_moddef

  handler systemInit "init" $ callback $ const $ do
    filter_init lpf_ax
    filter_init lpf_ay
    filter_init lpf_az

  handler a "accel" $ callback $ \a_samp -> do
    ax <- deref ((a_samp ~> A.sample) ~> XYZ.x)
    ay <- deref ((a_samp ~> A.sample) ~> XYZ.y)
    az <- deref ((a_samp ~> A.sample) ~> XYZ.z)
    filter_sample lpf_ax ax
    filter_sample lpf_ay ay
    filter_sample lpf_az az

  inMotion <- state "in_motion"
  handler motion "motion" $ callback $ refCopy inMotion

  handler newoutput "output" $ do
    e <- emitter c 1
    callbackV $ \t -> do
      bx <- filter_out lpf_ax
      by <- filter_out lpf_ay
      bz <- filter_out lpf_az
      o <- local $ iarray [ival bx, ival by, ival bz]

      comment "find argmax (abs o[i]): axis aligned with gravity vector"
      maxAccel <- local (ival (-1))
      maxAccelAxis <- local izero
      arrayMap $ \ i -> do
        curAccel <- fmap abs $ deref $ o ! i
        oldMax <- deref maxAccel
        when (curAccel >? oldMax) $ do
          store maxAccelAxis i
          store maxAccel curAccel

      comment "remove gravity vector from computed biases"
      axis <- deref maxAccelAxis
      axisAccel <- deref $ o ! axis
      store (o ! axis) ((axisAccel <? 0) ? (axisAccel + 9.80665, axisAccel - 9.80665))

      comment "emit estimated biases"
      moving <- deref inMotion
      cal <- mkCalibration (constRef o) (iNot moving) t
      emit e cal

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

accelCalibrate :: Calibrate (Struct "accelerometer_sample")
accelCalibrate = Calibrate aux
  where
  aux samp cal = do
    out <- local izero
    refCopy out samp
    v <- deref (cal ~> C.valid)
    when v $ do
      bx <- deref ((cal ~> C.bias) ~> XYZ.x)
      by <- deref ((cal ~> C.bias) ~> XYZ.y)
      bz <- deref ((cal ~> C.bias) ~> XYZ.z)
      ((out ~> A.sample) ~> XYZ.x) %= subtract bx
      ((out ~> A.sample) ~> XYZ.y) %= subtract by
      ((out ~> A.sample) ~> XYZ.z) %= subtract bz
      store (out ~> A.calibrated) true
    return (constRef out)

