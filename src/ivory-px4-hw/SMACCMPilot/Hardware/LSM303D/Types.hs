{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.LSM303D.Types where


import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

lsm303dTypesModule :: Module
lsm303dTypesModule = package "lsm303d_types" $ do
  defStruct (Proxy :: Proxy "lsm303d_sample")
  depend serializeModule
  wrappedPackMod lsm303dWrapper

[ivory|
struct lsm303d_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; acc_sample :: Array 3 (Stored IFloat) -- m/s/s
  ; mag_sample :: Array 3 (Stored IFloat) -- Gauss
  ; time       :: Stored ITime
  }
|]

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

lsm303dWrapper :: WrappedPackRep (Struct "lsm303d_sample")
lsm303dWrapper = wrapPackRep "lsm303d_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel  acc_sample
  , packLabel  mag_sample
  , packLabel' time packITime
  ]

instance Packable (Struct "lsm303d_sample") where
  packRep = wrappedPackRep lsm303dWrapper
