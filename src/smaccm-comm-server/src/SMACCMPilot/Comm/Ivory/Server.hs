{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Comm.Ivory.Server where

import Control.Monad (void)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Serialize
import SMACCMPilot.Comm.Ivory.Types
import SMACCMPilot.Comm.Ivory.Interface.ControllableVehicle.Producer

controllableVehicleProducerInput :: (ANat n)
                                 => ChanOutput (Array n (Stored Uint8))
                                 -> Tower e ( ChanOutput (Struct "waypoint")
                                            , ChanOutput (Struct "waypoint")
                                            , ChanOutput (Struct "heartbeat"))
controllableVehicleProducerInput frame_ch = do
  curr_waypt <- channel
  next_waypt <- channel
  heartbeat  <- channel

  monitor "controllableVehicleProducerInput" $ do
    handler frame_ch "parse_frame" $ do
      cw_e <- emitter (fst curr_waypt) 1
      nw_e <- emitter (fst next_waypt) 1
      hb_e <- emitter (fst heartbeat)  1
      callback $ \f -> do
        offs <- local izero
        void $ controllableVehicleProducerParser f offs $ ControllableVehicleProducer
          { currentWaypointValProducer = \v -> emit cw_e v >> return true
          , nextWaypointValProducer    = \v -> emit nw_e v >> return true
          , heartbeatProducer          = \v -> emit hb_e v >> return true
          }

  return (snd curr_waypt, snd next_waypt, snd heartbeat)



controllableVehicleProducerOutput :: (ANat n)
                                  => ChanOutput (Struct "waypoint")
                                  -> ChanOutput (Struct "waypoint")
                                  -> ChanOutput (Struct "heartbeat")
                                  -> Tower e (ChanOutput (Array n (Stored Uint8)))
controllableVehicleProducerOutput curr_waypt next_waypt heartbeat = do
  frame_ch <- channel

  monitor "controllableVehicleProducerOutput" $ do
    handler curr_waypt "curr_waypt" $ do
      e <- emitter (fst frame_ch) 1
      callback $ \w -> do
        f    <- local izero
        offs <- local izero
        let sender = controllableVehicleProducerSender f offs
        ok <- currentWaypointValProducer sender w
        when ok $ emit e (constRef f)

    handler next_waypt "next_waypt" $ do
      e <- emitter (fst frame_ch) 1
      callback $ \w -> do
        f    <- local izero
        offs <- local izero
        let sender = controllableVehicleProducerSender f offs
        ok <- nextWaypointValProducer sender w
        when ok $ emit e (constRef f)

    handler heartbeat "heartbeat" $ do
      e <- emitter (fst frame_ch) 1
      callback $ \w -> do
        f    <- local izero
        offs <- local izero
        let sender = controllableVehicleProducerSender f offs
        ok <- heartbeatProducer sender w
        when ok $ emit e (constRef f)

  return (snd frame_ch)

commTowerDependencies :: Tower e ()
commTowerDependencies = do
  towerModule serializeModule
  mapM_ towerArtifact serializeArtifacts
  mapM_ towerModule typeModules
  mapM_ towerDepends typeModules
