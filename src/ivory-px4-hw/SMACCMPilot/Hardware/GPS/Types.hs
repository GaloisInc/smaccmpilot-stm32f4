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
  wrappedPackMod gpsPositionWrapper

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

gpsPositionWrapper :: WrappedPackRep (Struct "position")
gpsPositionWrapper = wrapPackRep "position" $ packStruct
  [ packLabel fix
  , packLabel num_sv
  , packLabel dop
  , packLabel lat
  , packLabel lon
  , packLabel alt
  , packLabel vnorth
  , packLabel veast
  , packLabel vdown
  , packLabel vground
  , packLabel heading
  , packLabel' time packITime
  ]

instance Packable (Struct "position") where
  packRep = wrappedPackRep gpsPositionWrapper
