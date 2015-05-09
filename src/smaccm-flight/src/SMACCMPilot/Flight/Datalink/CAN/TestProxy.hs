{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxy
  ( app
  ) where

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Commsec.Sizes
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.Language
import Ivory.Tower
import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
import SMACCMPilot.Flight.Datalink.Commsec (padTower, unpadTower')
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)
import SMACCMPilot.Hardware.CAN

import Ivory.BSP.STM32.Driver.UART
import SMACCMPilot.Datalink.HXStream.Tower


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  fp <- fmap tofp getEnv

  can <- maybe (fail "fragmentation test requires a CAN peripheral") return $ fp_can fp
  (canRx, canTx, _, _) <- canTower tocc (can_periph can) 125000 (can_RX can) (can_TX can)

  monitor "can_init" $ handler systemInit "can_init" $ do
    callback $ const $ do
      let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
      canFilterInit (can_filters can)
        [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []
  cv_in <- channel
  cv_out <- channel

  uartDatalink tocc (fp_telem fp) 115200 (fst cv_out) (snd cv_in)

  canDatalink canTx canRx (fst cv_in) (snd cv_out)

  return ()
  where
  tocc = fp_clockconfig . tofp

canDatalink :: AbortableTransmit (Struct "can_message") (Stored IBool)
            -> ChanOutput (Struct "can_message")
            -> ChanInput PlaintextArray
            -> ChanOutput PlaintextArray
            -> Tower e ()
canDatalink tx rx assembled toFrag = do
  fragmentReceiver rx [fragmentReceiveHandler assembled s2cType]
  fragmentSenderBlind toFrag c2sType tx


uartDatalink :: (e -> ClockConfig)
             -> UART_Device
             -> Integer
             -> ChanInput PlaintextArray
             -> ChanOutput PlaintextArray
             -> Tower e ()
uartDatalink tocc uart baud input output = do
  (uarto, uarti) <- uartTower tocc
                              (uart_periph uart)
                              (uart_pins   uart)
                              baud
  padded_input_frames <- channel

  hxstreamDecodeTower "frame" uarti (fst padded_input_frames)

  input_frames <- channel

  unpadTower' (snd padded_input_frames) (fst input_frames)

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

  padded_output <- padTower output
  hxstreamEncodeTower "frame" padded_output uarto

