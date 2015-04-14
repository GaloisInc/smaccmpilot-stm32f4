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



data ControllableVehicleParams =
  ControllableVehicleParams
    { currentWaypoint :: Param (Struct "waypoint")
    , nextWaypoint    :: Param (Struct "waypoint")
    }

towerControllableVehicleParams :: Tower e ControllableVehicleParams
towerControllableVehicleParams = do
  currentWaypoint <- towerParam "current_waypoint" izero
  nextWaypoint    <- towerParam "next_waypoint"    izero
  return ControllableVehicleParams{..}

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
                          -> ControllableVehicleParams
                          -> ControllableVehicleStreams ChanOutput
                          -> Tower e ControllableVehicleProducer
controllableVehicleServer cvc params streams = do

  currentWaypointVal <- channel
  monitor "currentWaypointServer" $ do
    s <- paramState (currentWaypoint params)
    handler (currentWaypointGetConsumer cvc) "currentWaypointGet" $ do
      e <- emitter (fst currentWaypointVal) 1
      callback $ const $ emit e (constRef s)

  nextWaypointVal <- channel
  monitor "nextWaypointServer" $ do
    s <- paramState (nextWaypoint params)
    handler (nextWaypointGetConsumer cvc) "nextWaypointGet" $ do
      e <- emitter (fst nextWaypointVal) 1
      callback $ const $ emit e (constRef s)
    handler (nextWaypointSetConsumer cvc) "nextWaypointSet" $ do
      e <- paramEmitter (nextWaypoint params)
      callback $ \v -> emit e v

  let cvp :: ControllableVehicleProducer
      cvp = ControllableVehicleProducer
              { currentWaypointValProducer = snd currentWaypointVal
              , nextWaypointValProducer    = snd nextWaypointVal
              , heartbeatProducer          = heartbeat streams
              }
  return cvp
