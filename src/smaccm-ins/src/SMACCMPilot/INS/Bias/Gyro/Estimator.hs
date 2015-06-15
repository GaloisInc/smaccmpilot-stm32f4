{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Bias.Gyro.Estimator
  ( monitorGyroBiasEstimator
  , ivoryGyroBiasEstimator
  , GyroBiasEstimator(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import SMACCMPilot.INS.Filter


data GyroBiasEstimator =
  GyroBiasEstimator
    { gbe_init         :: forall eff . Ivory eff ()
    , gbe_gyro_sample  :: forall eff s
                        . ConstRef s (Array 3 (Stored IFloat))
                       -> Ivory eff ()
    , gbe_accel_sample :: forall eff s
                        . ConstRef s (Array 3 (Stored IFloat))
                       -> Ivory eff ()
    , gbe_output       :: forall eff s
                        . Ref s (Array 3 (Stored IFloat))
                       -> Ivory eff IBool
    }

monitorGyroBiasEstimator :: Monitor e GyroBiasEstimator
monitorGyroBiasEstimator = do
  n <- freshname "gbe"
  let (f, moddef) = ivoryGyroBiasEstimator (showUnique n)
  monitorModuleDef moddef
  return f

ivoryGyroBiasEstimator :: String -> (GyroBiasEstimator, ModuleDef)
ivoryGyroBiasEstimator n = (f, moddef)
  where
  gyro_threshold  = 25
  accel_threshold = 0.75

  f = GyroBiasEstimator
    { gbe_init         = do
        filter_init lpf_gx
        filter_init lpf_gy
        filter_init lpf_gz
        filter_init hpf_gx
        filter_init hpf_gy
        filter_init hpf_gz
        filter_init hpf_ax
        filter_init hpf_ay
        filter_init hpf_az
    , gbe_gyro_sample  = \g -> do
        gx <- deref (g ! 0)
        gy <- deref (g ! 1)
        gz <- deref (g ! 2)
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
    , gbe_accel_sample = \a -> do
        ax <- deref (a ! 0)
        ay <- deref (a ! 1)
        az <- deref (a ! 2)
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
    , gbe_output = \o -> do
        filter_out lpf_gx >>= store (o ! 0)
        filter_out lpf_gy >>= store (o ! 1)
        filter_out lpf_gz >>= store (o ! 2)
        a_t <- deref accel_threshold_ctr
        g_t <- deref gyro_threshold_ctr
        return (a_t >? 100 .&& g_t >? 100)
    }

  lowpass fn = ivoryRunningAverageFilter fn (Proxy :: Proxy 64)
  highpass fn = ivory2ndOrderFilter fn highPassButterworth

  (lpf_gx, lpf_gx_moddef) = lowpass (named "gx")
  (lpf_gy, lpf_gy_moddef) = lowpass (named "gy")
  (lpf_gz, lpf_gz_moddef) = lowpass (named "gz")
  (hpf_gx, hpf_gx_moddef) = highpass (named "gx")
  (hpf_gy, hpf_gy_moddef) = highpass (named "gy")
  (hpf_gz, hpf_gz_moddef) = highpass (named "gz")
  (hpf_ax, hpf_ax_moddef) = highpass (named "ax")
  (hpf_ay, hpf_ay_moddef) = highpass (named "ay")
  (hpf_az, hpf_az_moddef) = highpass (named "az")

  accel_threshold_ctr_area  :: MemArea (Stored Uint32)
  accel_threshold_ctr_area   = area (named "accel_threshold_counter") Nothing
  accel_threshold_ctr        = addrOf accel_threshold_ctr_area
  gyro_threshold_ctr_area   :: MemArea (Stored Uint32)
  gyro_threshold_ctr_area    = area (named "gyro_threshold_counter") Nothing
  gyro_threshold_ctr         = addrOf gyro_threshold_ctr_area

  moddef = do
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
  named s = n ++ "_" ++ s
