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
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Filter
import SMACCMPilot.Time

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
  n <- freshname "gbe"
  let named s = showUnique n ++ "_" ++ s

  let gyro_threshold  = 25
  let accel_threshold = 0.75

  let lowpass fn = ivoryRunningAverageFilter fn (Proxy :: Proxy 64)
  let highpass fn = ivory2ndOrderFilter fn highPassButterworth

  let (lpf_gx, lpf_gx_moddef) = lowpass (named "gx")
  let (lpf_gy, lpf_gy_moddef) = lowpass (named "gy")
  let (lpf_gz, lpf_gz_moddef) = lowpass (named "gz")
  let (hpf_gx, hpf_gx_moddef) = highpass (named "gx")
  let (hpf_gy, hpf_gy_moddef) = highpass (named "gy")
  let (hpf_gz, hpf_gz_moddef) = highpass (named "gz")
  let (hpf_ax, hpf_ax_moddef) = highpass (named "ax")
  let (hpf_ay, hpf_ay_moddef) = highpass (named "ay")
  let (hpf_az, hpf_az_moddef) = highpass (named "az")

  let accel_threshold_ctr_area  :: MemArea (Stored Uint32)
      accel_threshold_ctr_area   = area (named "accel_threshold_counter") Nothing
  let accel_threshold_ctr        = addrOf accel_threshold_ctr_area
  let gyro_threshold_ctr_area   :: MemArea (Stored Uint32)
      gyro_threshold_ctr_area    = area (named "gyro_threshold_counter") Nothing
  let gyro_threshold_ctr         = addrOf gyro_threshold_ctr_area

  monitorModuleDef $ do
    defMemArea accel_threshold_ctr_area
    defMemArea gyro_threshold_ctr_area
    lpf_gx_moddef
    lpf_gy_moddef
    lpf_gz_moddef
    hpf_gx_moddef
    hpf_gy_moddef
    hpf_gz_moddef
    hpf_ax_moddef
    hpf_ay_moddef
    hpf_az_moddef

  handler systemInit "init" $ callback $ const $ do
    filter_init lpf_gx
    filter_init lpf_gy
    filter_init lpf_gz
    filter_init hpf_gx
    filter_init hpf_gy
    filter_init hpf_gz
    filter_init hpf_ax
    filter_init hpf_ay
    filter_init hpf_az

  handler g "gyro" $ callback $ \g_samp -> do
    gx <- deref ((g_samp ~> G.sample) ~> XYZ.x)
    gy <- deref ((g_samp ~> G.sample) ~> XYZ.y)
    gz <- deref ((g_samp ~> G.sample) ~> XYZ.z)
    filter_sample lpf_gx gx
    filter_sample hpf_gx gx
    filter_sample lpf_gy gy
    filter_sample hpf_gy gy
    filter_sample lpf_gz gz
    filter_sample hpf_gz gz
    hgx <- filter_out hpf_gx
    hgy <- filter_out hpf_gy
    hgz <- filter_out hpf_gz
    hg_mag <- assign (hgx * hgx + hgy * hgy + hgz * hgz)
    ifte_ (hg_mag >? gyro_threshold)
          (store gyro_threshold_ctr 0)
          (gyro_threshold_ctr %= (+1))

  handler a "accel" $ callback $ \a_samp -> do
    ax <- deref ((a_samp ~> A.sample) ~> XYZ.x)
    ay <- deref ((a_samp ~> A.sample) ~> XYZ.y)
    az <- deref ((a_samp ~> A.sample) ~> XYZ.z)
    filter_sample hpf_ax ax
    filter_sample hpf_ay ay
    filter_sample hpf_az az
    hax <- filter_out hpf_ax
    hay <- filter_out hpf_ay
    haz <- filter_out hpf_az
    ha_mag <- assign (hax * hax + hay * hay + haz * haz)
    ifte_ (ha_mag >? accel_threshold)
          (store accel_threshold_ctr 0)
          (accel_threshold_ctr %= (+1))

  handler newoutput "output" $ do
    e <- emitter c 1
    callbackV $ \t -> do
      o <- local izero
      filter_out lpf_gx >>= store (o ! 0)
      filter_out lpf_gy >>= store (o ! 1)
      filter_out lpf_gz >>= store (o ! 2)
      a_t <- deref accel_threshold_ctr
      g_t <- deref gyro_threshold_ctr
      let done = a_t >? 100 .&& g_t >? 100
      cal <- mkCalibration (constRef o) done t
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
