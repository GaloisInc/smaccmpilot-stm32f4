{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.ControllableVehicle
  ( controllableVehicle
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Comm.Tower.Attr
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle


controllableVehicle :: forall e n
                     . (ANat n)
                    => ChanOutput (Array n (Stored Uint8))
                    -> Tower e ( ( ControllableVehicleAttrs Attr
                                 , ControllableVehicleStreams ChanInput
                                 )
                               , ChanOutput (Array n (Stored Uint8))
                               )
controllableVehicle input_frames = do

  attrs <- towerControllableVehicleAttrs initControllableVehicleAttrs
  streams <- towerControllableVehicleStreams

  cvc <- controllableVehicleConsumerInput input_frames
  cvp <- towerControllableVehicleServer cvc attrs (snd streams)
  output_frames <- controllableVehicleProducerOutput cvp

  return ((attrs, fst streams), output_frames)
