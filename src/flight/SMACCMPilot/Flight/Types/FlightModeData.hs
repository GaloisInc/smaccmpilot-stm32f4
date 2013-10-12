{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.FlightModeData
  ( FlightMode()
  , flightModeStabilize
  , flightModeAltHold
  , flightModeLoiter
  , fromFlightMode
  ) where

import Ivory.Language

newtype FlightMode = FlightMode Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

flightModeStabilize, flightModeAltHold, flightModeLoiter :: FlightMode
flightModeStabilize = FlightMode 0
flightModeAltHold   = FlightMode 1
flightModeLoiter    = FlightMode 2

fromFlightMode :: FlightMode -> Uint8
fromFlightMode (FlightMode fm) = fm
