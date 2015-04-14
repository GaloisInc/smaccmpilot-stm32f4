{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Comm.Ivory.Server where


import Control.Monad (void)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Serialize

import SMACCMPilot.Comm.Ivory.Attr
import SMACCMPilot.Comm.Ivory.Types
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle.Producer
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle.Consumer



data ControllableVehicleAttrs p =
  ControllableVehicleAttrs
    { currentWaypoint :: p (Struct "waypoint")
    , nextWaypoint    :: p (Struct "waypoint")
    }

towerControllableVehicleAttrs :: ControllableVehicleAttrs Init
                               -> Tower e (ControllableVehicleAttrs Attr)
towerControllableVehicleAttrs ivals = do
  p_currentWaypoint <- towerAttr "current_waypoint" (currentWaypoint ivals)
  p_nextWaypoint    <- towerAttr "next_waypoint"    (nextWaypoint ivals)
  return ControllableVehicleAttrs
    { currentWaypoint = p_currentWaypoint
    , nextWaypoint = p_nextWaypoint
    }

initControllableVehicleAttrs :: ControllableVehicleAttrs Init
initControllableVehicleAttrs = ControllableVehicleAttrs
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
                          -> ControllableVehicleAttrs Attr
                          -> ControllableVehicleStreams ChanOutput
                          -> Tower e ControllableVehicleProducer
controllableVehicleServer ControllableVehicleConsumer{..} ControllableVehicleAttrs{..} streams = do

  currentWaypointValProducer <- readableAttrServer
                                            currentWaypoint
                                            currentWaypointGetConsumer
  nextWaypointValProducer <- readwritableAttrServer
                                             nextWaypoint
                                             nextWaypointGetConsumer
                                             nextWaypointSetConsumer

  let heartbeatProducer = heartbeat streams
  return ControllableVehicleProducer{..}
