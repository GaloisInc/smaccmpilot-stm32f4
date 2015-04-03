
module SMACCMPilot.Comm.Ivory.Server where

import Ivory.Language
import Ivory.Tower
import Ivory.Serialize
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Comm.Ivory.Types
import SMACCMPilot.Comm.Ivory.Interface.ControllableVehicle

commTower :: Tower e ()
commTower = do
  towerModule serializeModule
  mapM_ towerArtifact serializeArtifacts
  mapM_ towerModule typeModules
  mapM_ towerDepends typeModules
