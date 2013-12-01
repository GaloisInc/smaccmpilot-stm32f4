
module SMACCMPilot.Flight.Types
  ( typeModules
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Types.ControlOutput (controlOutputTypeModule)
import SMACCMPilot.Flight.Types.OptFlow (optFlowTypeModule)
import SMACCMPilot.Flight.Types.Sensors (sensorsTypeModule)
import SMACCMPilot.Flight.Types.Motors (motorsTypeModule)
import SMACCMPilot.Flight.Types.UserInput (userInputTypeModule)
import SMACCMPilot.Flight.Types.FlightMode (flightModeTypeModule)
import SMACCMPilot.Flight.Types.GCSStreamTiming (gcsStreamTimingTypeModule)
import SMACCMPilot.Flight.Types.DataRate (dataRateTypeModule)
import SMACCMPilot.Flight.Types.RadioStat (radioStatTypeModule)
import SMACCMPilot.Flight.Types.RadioInfo (radioInfoTypeModule)
import SMACCMPilot.Flight.Types.MaybeFloat (maybeFloatModule)
import SMACCMPilot.Flight.Types.AltControlDebug (altControlDebugTypeModule)

typeModules :: [Module]
typeModules =
  [ controlOutputTypeModule
  , optFlowTypeModule
  , sensorsTypeModule
  , motorsTypeModule
  , userInputTypeModule
  , flightModeTypeModule
  , gcsStreamTimingTypeModule
  , dataRateTypeModule
  , radioStatTypeModule
  , radioInfoTypeModule
  , maybeFloatModule
  , altControlDebugTypeModule
  ]
