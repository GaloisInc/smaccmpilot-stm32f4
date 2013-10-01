{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.GPS.Types
  ( gpsTypesModule
  , module SMACCMPilot.Hardware.GPS.Types.GPSFix
  , module SMACCMPilot.Hardware.GPS.Types.Position
  ) where

import SMACCMPilot.Hardware.GPS.Types.GPSFix
import SMACCMPilot.Hardware.GPS.Types.Position

import Ivory.Language

gpsTypesModule :: Module
gpsTypesModule = package "gps_type" $ do
  defStruct (Proxy :: Proxy "position")

