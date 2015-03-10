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
  { coeff :: Array 6 (Stored Uint16)
  }

struct ms5611_sample
  { sample_temperature :: Stored Uint32
  ; sample_pressure    :: Stored Uint32
  ; sample_time        :: Stored ITime
  }
|]

ms5611TypesModule :: Module
ms5611TypesModule = package "ms5611_types" $ do
  defStruct (Proxy :: Proxy "ms5611_calibration")
  defStruct (Proxy :: Proxy "ms5611_sample")
