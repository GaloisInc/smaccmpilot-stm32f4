{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.FlightMode where

import Ivory.Language

flightModeTypeModule :: Module
flightModeTypeModule = package "flightmode_type" $ do
  defStruct (Proxy :: Proxy "flightmode")

flightModeStabilize, flightModeAltHold, flightModeLoiter :: Uint8
flightModeStabilize = 0
flightModeAltHold   = 1
flightModeLoiter    = 2

[ivory|
struct flightmode
  { armed    :: Stored IBool
  ; mode     :: Stored Uint8
  ; time     :: Stored Uint32
  }
|]

