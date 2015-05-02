
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.Plaintext
  ( plaintextDatalink
  ) where

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.Flight.Platform (UART_Device(..), ClockConfig)


plaintextDatalink :: (e -> ClockConfig)
                  -> UART_Device
                  -> Integer
                  -> ( ChanOutput CyphertextArray
                      -> Tower e (a, ChanOutput CyphertextArray))
                  -> Tower e a
plaintextDatalink tocc uart baud k = do
  (uarto, uarti) <- uartTower tocc
                              (uart_periph uart)
                              (uart_pins   uart)
                              baud
  input_frames <- channel

  hxstreamDecodeTower "frame" uarti (fst input_frames)

  (a, output_frames) <- k (snd input_frames)

  hxstreamEncodeTower "frame" output_frames uarto

  return a
