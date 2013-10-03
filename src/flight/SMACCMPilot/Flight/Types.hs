
module SMACCMPilot.Flight.Types
  ( typeModules
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Types.ControlOutput (controlOutputTypeModule)
import SMACCMPilot.Flight.Types.OptFlow (optFlowTypeModule)
import SMACCMPilot.Flight.Types.Position (positionTypeModule)
import SMACCMPilot.Flight.Types.Sensors (sensorsTypeModule)
import SMACCMPilot.Flight.Types.Motors (motorsTypeModule)
import SMACCMPilot.Flight.Types.UserInput (userInputTypeModule)
import SMACCMPilot.Flight.Types.FlightMode (flightModeTypeModule)
import SMACCMPilot.Flight.Types.GCSStreamTiming (gcsStreamTimingTypeModule)
import SMACCMPilot.Flight.Types.DataRate (dataRateTypeModule)
import SMACCMPilot.Flight.Types.RadioStat (radioStatTypeModule)
import SMACCMPilot.Flight.Types.RadioInfo (radioInfoTypeModule)

typeModules :: [Module]
typeModules =
  [ controlOutputTypeModule
  , optFlowTypeModule
  , positionTypeModule
  , sensorsTypeModule
  , motorsTypeModule
  , userInputTypeModule
  , flightModeTypeModule
  , gcsStreamTimingTypeModule
  , dataRateTypeModule
  , radioStatTypeModule
  , radioInfoTypeModule
  ]

