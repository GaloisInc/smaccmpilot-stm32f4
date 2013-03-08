
module SMACCMPilot.Flight.Types
  ( typeModules
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Types.ControlOutput (controlOutputTypeModule)
import SMACCMPilot.Flight.Types.MotorsOutput (motorsOutputTypeModule)
import SMACCMPilot.Flight.Types.OptFlow (optFlowTypeModule)
import SMACCMPilot.Flight.Types.Position (positionTypeModule)
import SMACCMPilot.Flight.Types.PositionEstimate (positionEstimateTypeModule)
import SMACCMPilot.Flight.Types.Sensors (sensorsTypeModule)
import SMACCMPilot.Flight.Types.Servo (servoTypeModule)
import SMACCMPilot.Flight.Types.UserInput (userInputTypeModule)


typeModules :: [Module]
typeModules =
  [ controlOutputTypeModule
  , motorsOutputTypeModule
  , optFlowTypeModule
  , positionTypeModule
  , positionEstimateTypeModule
  , sensorsTypeModule
  , servoTypeModule
  , userInputTypeModule
  ]

