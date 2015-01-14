{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.GPS.Types
  ( gpsTypesModule
  , module SMACCMPilot.Hardware.GPS.Types.GPSFix
  , module SMACCMPilot.Hardware.GPS.Types.Position
  ) where

import SMACCMPilot.Hardware.GPS.Types.GPSFix
import SMACCMPilot.Hardware.GPS.Types.Position

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

gpsTypesModule :: Module
gpsTypesModule = package "gps_type" $ do
  defStruct (Proxy :: Proxy "position")
  depend serializeModule
  incl gpsPositionPackRef
  incl gpsPositionUnpackRef

gpsPositionPackRef :: Def ('[ Ref s1 (CArray (Stored Uint8))
                            , Uint32
                            , ConstRef s2 (Struct "position")
                            ] :-> () )
gpsPositionPackRef = proc "gps_position_pack_ref" $ \ buf off msg -> body $ do
  t <- deref (msg ~> time)
  packRef buf (off + 0) (msg ~> fix)
  packRef buf (off + 1) (msg ~> num_sv)
  packRef buf (off + 2) (msg ~> dop)
  packRef buf (off + 6) (msg ~> lat)
  packRef buf (off + 10) (msg ~> lon)
  packRef buf (off + 14) (msg ~> alt)
  packRef buf (off + 18) (msg ~> vnorth)
  packRef buf (off + 22) (msg ~> veast)
  packRef buf (off + 26) (msg ~> vdown)
  packRef buf (off + 30) (msg ~> vground)
  packRef buf (off + 34) (msg ~> heading)
  pack buf (off + 38) (toIMicroseconds t)

gpsPositionUnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                              , Uint32
                              , Ref s2 (Struct "position")
                              ] :-> () )
gpsPositionUnpackRef = proc "gps_position_unpack_ref" $ \ buf off msg -> body $ do
  unpackRef buf (off + 0) (msg ~> fix)
  unpackRef buf (off + 1) (msg ~> num_sv)
  unpackRef buf (off + 2) (msg ~> dop)
  unpackRef buf (off + 6) (msg ~> lat)
  unpackRef buf (off + 10) (msg ~> lon)
  unpackRef buf (off + 14) (msg ~> alt)
  unpackRef buf (off + 18) (msg ~> vnorth)
  unpackRef buf (off + 22) (msg ~> veast)
  unpackRef buf (off + 26) (msg ~> vdown)
  unpackRef buf (off + 30) (msg ~> vground)
  unpackRef buf (off + 34) (msg ~> heading)
  t <- unpack buf (off + 38)
  store (msg ~> time) (fromIMicroseconds (t :: Sint64))

instance SerializableRef (Struct "position") where
  packRef = call_ gpsPositionPackRef
  unpackRef = call_ gpsPositionUnpackRef
