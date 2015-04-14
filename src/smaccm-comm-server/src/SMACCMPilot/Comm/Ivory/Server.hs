{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Comm.Ivory.Server where


import Control.Monad (void)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Serialize

import SMACCMPilot.Comm.Ivory.Param
import SMACCMPilot.Comm.Ivory.Types
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle.Producer
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle.Consumer



data ControllableVehicleParams p =
  ControllableVehicleParams
    { currentWaypoint :: p (Struct "waypoint")
    , nextWaypoint    :: p (Struct "waypoint")
    }

towerControllableVehicleParams :: ControllableVehicleParams Init
                               -> Tower e (ControllableVehicleParams Param)
towerControllableVehicleParams ivals = do
  p_currentWaypoint <- towerParam "current_waypoint" (currentWaypoint ivals)
  p_nextWaypoint    <- towerParam "next_waypoint"    (nextWaypoint ivals)
  return ControllableVehicleParams
    { currentWaypoint = p_currentWaypoint
    , nextWaypoint = p_nextWaypoint
    }

initControllableVehicleParams :: ControllableVehicleParams Init
initControllableVehicleParams = ControllableVehicleParams
  { currentWaypoint = izero
  , nextWaypoint = izero
  }

data ControllableVehicleStreams c =
  ControllableVehicleStreams
    { heartbeat :: c (Struct "heartbeat")
    }

towerControllableVehicleStreams :: Tower e ( ControllableVehicleStreams ChanInput
                                           , ControllableVehicleStreams ChanOutput)
towerControllableVehicleStreams = do
  c_heartbeat <- channel
  return ( ControllableVehicleStreams
            { heartbeat = fst c_heartbeat
            }
         , ControllableVehicleStreams
            { heartbeat = snd c_heartbeat
            }
         )

controllableVehicleServer :: ControllableVehicleConsumer
                          -> ControllableVehicleParams Param
                          -> ControllableVehicleStreams ChanOutput
                          -> Tower e ControllableVehicleProducer
controllableVehicleServer ControllableVehicleConsumer{..} params streams = do

  currentWaypointValProducer <- readableParamServer (currentWaypoint params)
                                            currentWaypointGetConsumer
  nextWaypointValProducer <- readwritableParamServer (nextWaypoint params)
                                             nextWaypointGetConsumer
                                             nextWaypointSetConsumer

  let heartbeatProducer = heartbeat streams
  return ControllableVehicleProducer{..}
