{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.ControllableVehicle
  ( controllableVehicle
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.RingBuffer

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
  buffered_input <- channel

  p <- period (Milliseconds 5)

  monitor "frame_input_buffer" $ do
    (rb :: RingBuffer 4 (Array n (Stored Uint8))) <- monitorRingBuffer "inputbuffer"
    handler input_frames "input_frames" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
    handler p "periodic_input_frame_pop" $ do
      e <- emitter (fst buffered_input) 1
      callback $ const $ do
        v <- local izero
        got <- ringbuffer_pop rb v
        ifte_ got (emit e (constRef v)) (return ())

  attrs <- towerControllableVehicleAttrs initControllableVehicleAttrs
  streams <- towerControllableVehicleStreams

  cvc <- controllableVehicleConsumerInput (snd buffered_input)
  cvp <- towerControllableVehicleServer cvc attrs (snd streams)
  output_frames <- controllableVehicleProducerOutput cvp

  return ((attrs, fst streams), output_frames)
