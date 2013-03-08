{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.Control.Altitude where

import Ivory.Language
import SMACCMPilot.Util.IvoryHelpers

import SMACCMPilot.Param

import SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Flight.Types.UserInput        as IN
import qualified SMACCMPilot.Flight.Types.PositionEstimate as POS
import qualified SMACCMPilot.Flight.Types.Sensors          as SEN
import qualified SMACCMPilot.Flight.Types.ControlOutput    as OUT




