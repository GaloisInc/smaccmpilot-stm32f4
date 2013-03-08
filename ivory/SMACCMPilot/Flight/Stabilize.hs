
module SMACCMPilot.Flight.Stabilize
  ( stabilizeModules
  ) where

import Ivory.Language
import SMACCMPilot.Flight.Stabilize.ControlLoops
import SMACCMPilot.Flight.Stabilize.PID

stabilizeModules :: [Module]
stabilizeModules = [ stabilizeControlLoopsModule, stabilizePIDModule ]
