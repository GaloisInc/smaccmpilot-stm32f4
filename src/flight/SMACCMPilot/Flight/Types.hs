
module SMACCMPilot.Flight.Types
  ( typeModules
  ) where

import Ivory.Language

import SMACCMPilot.Flight.Types.ControlOutput (controlOutputTypeModule)
import SMACCMPilot.Flight.Types.OptFlow (optFlowTypeModule)
import SMACCMPilot.Flight.Types.Sensors (sensorsTypeModule)
import SMACCMPilot.Flight.Types.Motors (motorsTypeModule)
import SMACCMPilot.Flight.Types.UserInput (userInputTypeModule)
import SMACCMPilot.Flight.Types.GCSStreamTiming (gcsStreamTimingTypeModule)
import SMACCMPilot.Flight.Types.RadioStat (radioStatTypeModule)
import SMACCMPilot.Flight.Types.RadioInfo (radioInfoTypeModule)
import SMACCMPilot.Flight.Types.MaybeFloat (maybeFloatModule)
import SMACCMPilot.Flight.Types.AltControlDebug (altControlDebugTypeModule)
import SMACCMPilot.Flight.Types.ControlLaw (controlLawTypeModule)
import SMACCMPilot.Flight.Types.ControlLawRequest (controlLawRequestTypeModule)
import SMACCMPilot.Flight.Types.AttControlDebug (attControlDebugTypeModule)
import SMACCMPilot.Flight.Types.PosControlDebug (posControlDebugTypeModule)
import SMACCMPilot.Flight.Types.ControlSetpoint (controlSetpointTypeModule)
import SMACCMPilot.Flight.Types.NavCommand (navCommandTypeModule)
import SMACCMPilot.Flight.Types.NavLaw (navLawTypeModule)

typeModules :: [Module]
typeModules =
  [ controlOutputTypeModule
  , optFlowTypeModule
  , sensorsTypeModule
  , motorsTypeModule
  , userInputTypeModule
  , gcsStreamTimingTypeModule
  , radioStatTypeModule
  , radioInfoTypeModule
  , maybeFloatModule
  , altControlDebugTypeModule
  , controlLawTypeModule
  , controlLawRequestTypeModule
  , attControlDebugTypeModule
  , posControlDebugTypeModule
  , controlSetpointTypeModule
  , navCommandTypeModule
  , navLawTypeModule
  ]
