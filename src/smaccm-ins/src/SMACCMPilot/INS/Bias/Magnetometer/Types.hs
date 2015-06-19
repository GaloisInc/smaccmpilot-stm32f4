{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.INS.Bias.Magnetometer.Types where

import Ivory.Language

[ivory|
struct mag_bias_sums
  { x_sq    :: Stored IFloat
  ; y_sq    :: Stored IFloat
  ; z_sq    :: Stored IFloat
  ; x       :: Stored IFloat
  ; y       :: Stored IFloat
  ; z       :: Stored IFloat
  ; xy      :: Stored IFloat
  ; xz      :: Stored IFloat
  ; yz      :: Stored IFloat
  ; x_sumsq :: Stored IFloat
  ; y_sumsq :: Stored IFloat
  ; z_sumsq :: Stored IFloat
  ; m       :: Stored Uint32
  }

struct mag_diversity_buf
  { buf :: Array 64 (Array 3 (Stored IFloat))
  }
|]

-- MBE Buf Depth and distance_threshold (see below) are decided fairly
-- arbitrarily. These numbers *seem to work* for a small number of tests.
type MBEBufDepth = 64

magnetometerBiasTypesModule :: Module
magnetometerBiasTypesModule = package "magnetometer_bias_types" $ do
  defStruct (Proxy :: Proxy "mag_bias_sums")
  defStruct (Proxy :: Proxy "mag_diversity_buf")

