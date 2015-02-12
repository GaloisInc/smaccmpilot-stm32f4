{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MS5611.Types where

import Ivory.Language
import Ivory.Serialize
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
  { initfail   :: Stored IBool
  ; sampfail   :: Stored IBool
  ; pressure    :: Stored IFloat -- mbar
  ; temperature :: Stored IFloat -- deg celsius
  ; time        :: Stored ITime
  }
|]

ms5611TypesModule :: Module
ms5611TypesModule = package "ms5611_types" $ do
  defStruct (Proxy :: Proxy "ms5611_calibration")
  defStruct (Proxy :: Proxy "ms5611_sample")
  defStruct (Proxy :: Proxy "ms5611_measurement")
  depend serializeModule
  wrappedPackMod ms5611Wrapper

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

ms5611Wrapper :: WrappedPackRep (Struct "ms5611_measurement")
ms5611Wrapper = wrapPackRep "ms5611_measurement" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' sampfail packIBool
  , packLabel pressure
  , packLabel temperature
  , packLabel' time packITime
  ]

instance Packable (Struct "ms5611_measurement") where
  packRep = wrappedPackRep ms5611Wrapper
