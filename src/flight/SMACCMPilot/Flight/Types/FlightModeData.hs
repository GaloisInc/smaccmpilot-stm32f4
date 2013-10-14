{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.FlightModeData
  ( FlightMode()
  , flightModeStabilize
  , flightModeAltHold
  , flightModeAuto
  , fromFlightMode
  , flightModes
  ) where

import Ivory.Language

newtype FlightMode = FlightMode Uint8
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

flightModeStabilize, flightModeAltHold, flightModeAuto :: FlightMode
flightModeStabilize = FlightMode 0
flightModeAltHold   = FlightMode 1
flightModeAuto      = FlightMode 2

fromFlightMode :: FlightMode -> Uint8
fromFlightMode (FlightMode fm) = fm

flightModes :: [FlightMode]
flightModes = [ flightModeStabilize, flightModeAltHold, flightModeAuto ]
