{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.UART
  ( uartDatalink
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

  bufferChans (snd input_frames)
  -- Buffering timing analysis:
  -- Worst case: 115200 baud
  -- 10 bits per byte (UART framing) = 11520 bytes per second
  -- 96+3 bytes per hx frame = 117 frames per second
  -- 117 frames per second = 8.6ms per frame
                                       (Milliseconds 5)
  -- buffer depth of 4: we will never use more than 2, and it will fit 3.
                                       (Proxy :: Proxy 4)
                                       input

  airDataEncodeTower
    "frame"
    output
    uarto
    (Milliseconds 5)
    (Proxy :: Proxy 4)
  return mon
