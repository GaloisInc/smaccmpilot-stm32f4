{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.ControllableVehicle
  ( controllableVehicle
  , controllableVehicle'
  , controllableVehicleAPI
  , CVAPI
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Comm.Tower.Attr
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

type CVAPI = (ControllableVehicleAttrs Attr, (ControllableVehicleStreams ChanInput, ControllableVehicleStreams ChanOutput))


controllableVehicleAPI :: Tower e CVAPI
controllableVehicleAPI = do
  attrs   <- towerControllableVehicleAttrs initControllableVehicleAttrs
  streams <- towerControllableVehicleStreams
  return (attrs, streams)

controllableVehicle' :: forall e n
                     . (ANat n)
                    => ChanOutput ('Array n ('Stored Uint8))
                    -> CVAPI
                    -> Tower e ( ChanOutput ('Array n ('Stored Uint8)))
controllableVehicle' input_frames (attrs, streams) = do

  cvc <- controllableVehicleConsumerInput input_frames
  cvp <- towerControllableVehicleServer cvc attrs (snd streams)
  output_frames <- controllableVehicleProducerOutput cvp

  return output_frames

controllableVehicle :: forall e n
                     . (ANat n)
                    => ChanOutput ('Array n ('Stored Uint8))
                    -> Tower e ( ( ControllableVehicleAttrs Attr
                                 , ControllableVehicleStreams ChanInput
                                 )
                               , ChanOutput ('Array n ('Stored Uint8))
                               )
controllableVehicle input_frames = do
  (attrs, streams) <- controllableVehicleAPI
  output_frames <- controllableVehicle' input_frames (attrs, streams)
  return ((attrs, fst streams), output_frames)
