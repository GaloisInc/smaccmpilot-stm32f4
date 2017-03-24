{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.Sensors.DetectMotion where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import SMACCMPilot.Flight.Filter

detectMotion :: ChanOutput ('Struct "gyroscope_sample")
             -> ChanOutput ('Struct "accelerometer_sample")
             -> ChanInput ('Stored IBool)
             -> Tower e ()
detectMotion g a res = monitor "detectMotion" $ do
  n <- freshname "detectMotion"
  let named s = showUnique n ++ "_" ++ s

  let gyro_threshold  = 0.25
  let accel_threshold = 0.25

  let highpass fn = ivory2ndOrderFilter fn highPassButterworth

  let (hpf_gx, hpf_gx_moddef) = highpass (named "gx")
  let (hpf_gy, hpf_gy_moddef) = highpass (named "gy")
  let (hpf_gz, hpf_gz_moddef) = highpass (named "gz")
  let (hpf_ax, hpf_ax_moddef) = highpass (named "ax")
  let (hpf_ay, hpf_ay_moddef) = highpass (named "ay")
  let (hpf_az, hpf_az_moddef) = highpass (named "az")

  monitorModuleDef $ do
    hpf_gx_moddef
    hpf_gy_moddef
    hpf_gz_moddef
    hpf_ax_moddef
    hpf_ay_moddef
    hpf_az_moddef

  accel_threshold_ctr <- stateInit "accel_threshold_counter" (izero :: Init ('Stored Uint32))
  gyro_threshold_ctr <- stateInit "gyro_threshold_counter" (izero :: Init ('Stored Uint32))

  handler systemInit (named "init") $ callback $ const $ do
    filter_init hpf_gx
    filter_init hpf_gy
    filter_init hpf_gz
    filter_init hpf_ax
    filter_init hpf_ay
    filter_init hpf_az

  let reportMotion :: (GetAlloc eff ~ 'Scope s)
                   => Emitter ('Stored IBool)
                   -> Ivory eff ()
      reportMotion e = do
        a_t <- deref accel_threshold_ctr
        g_t <- deref gyro_threshold_ctr
        emitV e $ a_t <=? 200 .|| g_t <=? 200

  handler g (named "gyro") $ do
    callback $ \g_samp -> do
      gx <- deref ((g_samp ~> G.sample) ~> XYZ.x)
      gy <- deref ((g_samp ~> G.sample) ~> XYZ.y)
      gz <- deref ((g_samp ~> G.sample) ~> XYZ.z)
      filter_sample hpf_gx gx
      filter_sample hpf_gy gy
      filter_sample hpf_gz gz
      hgx <- filter_out hpf_gx
      hgy <- filter_out hpf_gy
      hgz <- filter_out hpf_gz
      hg_mag <- assign (hgx * hgx + hgy * hgy + hgz * hgz)
      ifte_ (hg_mag >? gyro_threshold)
            (store gyro_threshold_ctr 0)
            (gyro_threshold_ctr %= (+1))

  handler a (named "accel") $ do
    e <- emitter res 1
    callback $ \a_samp -> do
      ax <- deref ((a_samp ~> A.sample) ~> XYZ.x)
      ay <- deref ((a_samp ~> A.sample) ~> XYZ.y)
      az <- deref ((a_samp ~> A.sample) ~> XYZ.z)
      filter_sample hpf_ax ax
      filter_sample hpf_ay ay
      filter_sample hpf_az az
      hax <- filter_out hpf_ax
      hay <- filter_out hpf_ay
      haz <- filter_out hpf_az
      ha_mag <- assign ((hax * hax + hay * hay + haz * haz) - 9.80665^(2::Int))
      ifte_ (ha_mag >? accel_threshold)
            (store accel_threshold_ctr 0)
            (accel_threshold_ctr %= (+1))
      reportMotion e
