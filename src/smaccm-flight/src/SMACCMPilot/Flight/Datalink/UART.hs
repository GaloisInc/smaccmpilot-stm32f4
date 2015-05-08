{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.UART
  ( uartDatalink
  ) where

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.Flight.Platform (UART_Device(..), ClockConfig)

uartDatalink :: (e -> ClockConfig)
             -> UART_Device
             -> Integer
             -> ( ChanOutput CyphertextArray
                 -> Tower e (a, ChanOutput CyphertextArray))
             -> Tower e a
uartDatalink tocc uart baud k = do
  (uarto, uarti) <- uartTower tocc
                              (uart_periph uart)
                              (uart_pins   uart)
                              baud
  input_frames <- channel

  hxstreamDecodeTower "frame" uarti (fst input_frames)
  -- TODO XXX FIXME: buffer input frames.

  (a, output_frames) <- k (snd input_frames)

  hxstreamEncodeTower "frame" output_frames uarto

  return a
