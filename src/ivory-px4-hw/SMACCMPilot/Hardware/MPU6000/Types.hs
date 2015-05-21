{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.MPU6000.Types where

import Ivory.Language
import Ivory.Serialize

[ivory|
struct mpu6000_response
  { ax   :: Stored Sint16
  ; ay   :: Stored Sint16
  ; az   :: Stored Sint16
  ; temp :: Stored Uint16
  ; gx   :: Stored Sint16
  ; gy   :: Stored Sint16
  ; gz   :: Stored Sint16
  }
|]

mpu6000ResponseWrapper :: WrappedPackRep (Struct "mpu6000_response")
mpu6000ResponseWrapper = wrapPackRep "mpu6000_response" $ packStruct
  [ packLabel ax
  , packLabel ay
  , packLabel az
  , packLabel temp
  , packLabel gx
  , packLabel gy
  , packLabel gz
  ]

mpu6000ResponseTypesModule :: Module
mpu6000ResponseTypesModule = package "mpu6000_response_types" $ do
  defStruct (Proxy :: Proxy "mpu6000_response")
  depend serializeModule
  wrappedPackMod mpu6000ResponseWrapper

instance Packable (Struct "mpu6000_response") where
  packRep = wrappedPackRep mpu6000ResponseWrapper



