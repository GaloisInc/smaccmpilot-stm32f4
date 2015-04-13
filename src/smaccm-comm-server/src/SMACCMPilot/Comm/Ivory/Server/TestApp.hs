{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Comm.Ivory.Server.TestApp
  ( app
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import SMACCMPilot.Hardware.Tests.Platforms

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.HXStream.Tower
import Ivory.BSP.STM32.Driver.RingBuffer

import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle.Producer

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do

  (uarti, uarto) <- px4ConsoleTower topx4


  input_frames_unbuffered <- channel
  hxstreamDecodeTower "frame" uarti (fst input_frames_unbuffered)

  input_frames <- channel

  p <- period (Milliseconds 5)
  monitor "inputbuffer" $ do
    (rb :: RingBuffer 4 CyphertextArray) <- monitorRingBuffer "inputbuffer"
    handler (snd input_frames_unbuffered) "input_frames" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()

    handler p "periodic_input_pop" $ do
      e <- emitter (fst input_frames) 1
      callback $ const $ do
        v <- local izero
        got <- ringbuffer_pop rb v
        when got $ emit e (constRef v)

  cvp <- controllableVehicleProducerInput (snd input_frames)
  (output_frames :: ChanOutput CyphertextArray) <-
        controllableVehicleProducerOutput cvp

  hxstreamEncodeTower "frame" output_frames uarto
