{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.INS.Bias.Gyro
  ( calcGyroBiasTower
  , calcGyroBiasTower'
  , gyroCalibrate
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Filter
import SMACCMPilot.Time

calcGyroBiasTower :: ChanOutput (Struct "gyroscope_sample")
                  -> ChanOutput (Stored IBool)
                  -> Tower e (ChanOutput (Struct "xyz_calibration"))
calcGyroBiasTower g motion = do
  cal <- channel
  p <- period (Milliseconds 1000)
  calcGyroBiasTower' g motion (fst cal) p
  return (snd cal)

calcGyroBiasTower' :: ChanOutput (Struct "gyroscope_sample")
                   -> ChanOutput (Stored IBool)
                   -> ChanInput  (Struct "xyz_calibration")
                   -> ChanOutput (Stored ITime)
                   -> Tower e ()
calcGyroBiasTower' g motion c newoutput = monitor "calcGyroBias" $ do
  n <- freshname "gbe"
  let named s = showUnique n ++ "_" ++ s

  let lowpass fn = ivoryRunningAverageFilter fn (Proxy :: Proxy 64)

  let (lpf_gx, lpf_gx_moddef) = lowpass (named "gx")
  let (lpf_gy, lpf_gy_moddef) = lowpass (named "gy")
  let (lpf_gz, lpf_gz_moddef) = lowpass (named "gz")

  monitorModuleDef $ do
    lpf_gx_moddef
    lpf_gy_moddef
    lpf_gz_moddef

  handler systemInit "init" $ callback $ const $ do
    filter_init lpf_gx
    filter_init lpf_gy
    filter_init lpf_gz

  handler g "gyro" $ callback $ \g_samp -> do
    gx <- deref ((g_samp ~> G.sample) ~> XYZ.x)
    gy <- deref ((g_samp ~> G.sample) ~> XYZ.y)
    gz <- deref ((g_samp ~> G.sample) ~> XYZ.z)
    filter_sample lpf_gx gx
    filter_sample lpf_gy gy
    filter_sample lpf_gz gz

  inMotion <- state "in_motion"
  handler motion "motion" $ callback $ refCopy inMotion

  handler newoutput "output" $ do
    e <- emitter c 1
    callbackV $ \t -> do
      o <- local izero
      filter_out lpf_gx >>= store (o ! 0)
      filter_out lpf_gy >>= store (o ! 1)
      filter_out lpf_gz >>= store (o ! 2)
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
