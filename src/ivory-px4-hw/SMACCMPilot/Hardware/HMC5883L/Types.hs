{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.HMC5883L.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

hmc5883lTypesModule :: Module
hmc5883lTypesModule = package "hmc5883l_types" $ do
  defStruct (Proxy :: Proxy "hmc5883l_sample")
  depend serializeModule
  wrappedPackMod hmc5883lWrapper

[ivory|
struct hmc5883l_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- Gauss
  ; time       :: Stored ITime
  }
|]

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

hmc5883lWrapper :: WrappedPackRep (Struct "hmc5883l_sample")
hmc5883lWrapper = wrapPackRep "hmc5883l_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel sample
  , packLabel' time packITime
  ]

instance Packable (Struct "hmc5883l_sample") where
  packRep = wrappedPackRep hmc5883lWrapper
