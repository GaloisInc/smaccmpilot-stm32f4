
module SMACCMPilot.Flight.Control
  ( controlModules
  ) where

import Ivory.Language
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Control.AltHold

controlModules :: [Module]
controlModules = [ stabilizeControlLoopsModule
                 , controlPIDModule
                 , altHoldModule
                 ]
