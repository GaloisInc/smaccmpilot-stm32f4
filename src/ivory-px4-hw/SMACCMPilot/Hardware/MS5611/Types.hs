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
|]

[ivory|
struct ms5611_sample
  { temperature :: Stored Uint32
  ; pressure    :: Stored Uint32
  ; time        :: Stored ITime
  }
|]

ms5611TypesModule :: Module
ms5611TypesModule = package "ms5611_types" $ do
  defStruct (Proxy :: Proxy "ms5611_calibration")
  defStruct (Proxy :: Proxy "ms5611_sample")

