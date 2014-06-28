{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MS5611.Types where

import Ivory.Language
import Ivory.Tower.Types.Time

[ivory|
struct ms5611_calibration
  { coeff1  :: Stored Uint16
  ; coeff2  :: Stored Uint16
  ; coeff3  :: Stored Uint16
  ; coeff4  :: Stored Uint16
  ; coeff5  :: Stored Uint16
  ; coeff6  :: Stored Uint16
  }

struct ms5611_sample
  { sample_temperature :: Stored Uint32
  ; sample_pressure    :: Stored Uint32
  ; sample_time        :: Stored ITime
  }

struct ms5611_measurement
  { init_fail   :: Stored IBool
  ; samp_fail   :: Stored IBool
  ; temperature :: Stored IFloat -- deg celsius
  ; pressure    :: Stored IFloat -- mbar
  ; time        :: Stored ITime
  }
|]

ms5611TypesModule :: Module
ms5611TypesModule = package "ms5611_types" $ do
  defStruct (Proxy :: Proxy "ms5611_calibration")
  defStruct (Proxy :: Proxy "ms5611_sample")
  defStruct (Proxy :: Proxy "ms5611_measurement")

