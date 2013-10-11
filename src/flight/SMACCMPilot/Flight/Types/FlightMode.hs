{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.FlightMode where

import Ivory.Language

flightModeTypeModule :: Module
flightModeTypeModule = package "flightmode_type" $ do
  defStruct (Proxy :: Proxy "flightmode")

flightModeStabilize, flightModeAltHold, flightModeAuto :: Uint8
flightModeStabilize = 0
flightModeAltHold   = 1
flightModeAuto      = 2

-- | List of all valid flight modes.
flightModes :: [Uint8]
flightModes =
  [ flightModeStabilize
  , flightModeAltHold
  , flightModeAuto
  ]

[ivory|
struct flightmode
  { armed    :: Stored IBool
  ; mode     :: Stored Uint8
  ; time     :: Stored Uint32
  }
|]

