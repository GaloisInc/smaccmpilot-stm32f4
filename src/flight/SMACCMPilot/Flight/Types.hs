
module SMACCMPilot.Flight.Types
  ( typeModules
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Types.ControlOutput (controlOutputTypeModule)
import SMACCMPilot.Flight.Types.OptFlow (optFlowTypeModule)
import SMACCMPilot.Flight.Types.Position (positionTypeModule)
import SMACCMPilot.Flight.Types.PositionEstimate (positionEstimateTypeModule)
import SMACCMPilot.Flight.Types.Sensors (sensorsTypeModule)
import SMACCMPilot.Flight.Types.Servos (servosTypeModule)
import SMACCMPilot.Flight.Types.UserInput (userInputTypeModule)
import SMACCMPilot.Flight.Types.FlightMode (flightModeTypeModule)
import SMACCMPilot.Flight.Types.GCSStreamTiming (gcsStreamTimingTypeModule)


typeModules :: [Module]
typeModules =
  [ controlOutputTypeModule
  , optFlowTypeModule
  , positionTypeModule
  , positionEstimateTypeModule
  , sensorsTypeModule
  , servosTypeModule
  , userInputTypeModule
  , flightModeTypeModule
  , gcsStreamTimingTypeModule
  ]

