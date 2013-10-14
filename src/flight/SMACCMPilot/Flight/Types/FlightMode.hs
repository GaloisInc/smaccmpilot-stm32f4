{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.FlightMode where

import Ivory.Language

import SMACCMPilot.Flight.Types.FlightModeData

[ivory|
struct flightmode
  { armed    :: Stored IBool
  ; mode     :: Stored FlightMode
  ; time     :: Stored Uint32
  }
|]

flightModeTypeModule :: Module
flightModeTypeModule = package "flightmode_type" $ do
  defStruct (Proxy :: Proxy "flightmode")
