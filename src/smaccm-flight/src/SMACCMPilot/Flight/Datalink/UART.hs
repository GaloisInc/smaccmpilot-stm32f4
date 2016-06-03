{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.UART
  ( uartDatalink
  , frameBuffer
  , frameBuffer'
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.RingBuffer

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.UART.DMA
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.Flight.Platform (UART_Device(..), ClockConfig)

uartDatalink :: (e -> ClockConfig)
             -> UART_Device
             -> Integer
             -> ChanInput CyphertextArray
             -> ChanOutput CyphertextArray
             -> Tower e (Monitor e ())
uartDatalink tocc uart baud input output = do
  let ps = uart_pins uart
  (uarto, uarti, mon) <- case uart_periph uart of
    Left  u -> uartTower tocc u ps baud
    Right u -> dmaUARTTower tocc u ps baud (Proxy :: Proxy HXCyphertext)

  input_frames <- channel

  airDataDecodeTower "frame" uarti (fst input_frames)

  frameBuffer' (snd input_frames)
  -- Buffering timing analysis:
  -- Worst case: 115200 baud
  -- 10 bits per byte (UART framing) = 11520 bytes per second
  -- 96+3 bytes per hx frame = 117 frames per second
  -- 117 frames per second = 8.6ms per frame
                                       (Milliseconds 5)
  -- buffer depth of 4: we will never use more than 2, and it will fit 3.
                                       (Proxy :: Proxy 4)
                                       input

  airDataEncodeTower "frame" output uarto
  return mon


frameBuffer :: forall a t n e
             . (IvoryArea a, IvoryZero a, Time t, ANat n)
            => ChanOutput a
            -> t
            -> Proxy n
            -> Tower e (ChanOutput a)
frameBuffer input pop_period _buf_size = do
  out <- channel
  frameBuffer' input pop_period _buf_size (fst out)
  return (snd out)

frameBuffer' :: forall a t n e
             . (IvoryArea a, IvoryZero a, Time t, ANat n)
            => ChanOutput a
            -> t
            -> Proxy n
            -> ChanInput a
            -> Tower e ()
frameBuffer' input pop_period _buf_size out = do

  p <- period pop_period
  monitor "frameBuffer" $ do
    (rb :: RingBuffer n a) <- monitorRingBuffer "frameBuffer"
    handler input "push" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
    handler p "periodic_pop" $ do
      e <- emitter out 1
      callback $ const $ do
        v <- local izero
        got <- ringbuffer_pop rb v
        ifte_ got (emit e (constRef v)) (return ())
