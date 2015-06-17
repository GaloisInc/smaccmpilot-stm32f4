{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxy
  ( app
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import SMACCMPilot.Hardware.CAN

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
import SMACCMPilot.Flight.Datalink.Commsec (padTower', unpadTower')
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Tower
import SMACCMPilot.Datalink.Mode


import Ivory.BSP.STM32.Driver.UART
import SMACCMPilot.Datalink.HXStream.Tower


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  fp <- fmap tofp getEnv

  can <- maybe (fail "CAN test proxy test requires a CAN peripheral") return $
               fp_can fp
  (canRx, canTx, _, _) <- canTower tocc (can_periph can) 125000 (can_RX can) (can_TX can)

  monitor "can_init" $ handler systemInit "can_init" $ do
    callback $ const $ do
      let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
      canFilterInit (can_filters can)
        [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []


  s2c_pt_from_uart <- channel
  s2c_ct_from_uart <- channel
  c2s_from_can <- channel

  cv_producer <- controllableVehicleProducerInput (snd c2s_from_can)
  c2s_pt_to_uart <- controllableVehicleProducerOutput cv_producer
  cv_consumer <- controllableVehicleConsumerInput (snd s2c_pt_from_uart)
  s2c_to_can <- controllableVehicleConsumerOutput cv_consumer

  c2s_ct_to_uart <- channel

  datalinkEncode todl c2s_pt_to_uart (fst c2s_ct_to_uart)
  datalinkDecode todl (snd s2c_ct_from_uart) (fst s2c_pt_from_uart)

  uartDatalink tocc (fp_telem fp) 115200 (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)
  canDatalink canTx canRx (fst c2s_from_can) s2c_to_can

  return ()
  where
  tocc = fp_clockconfig . tofp
  todl = fp_datalink . tofp

canDatalink :: AbortableTransmit (Struct "can_message") (Stored IBool)
            -> ChanOutput (Struct "can_message")
            -> ChanInput PlaintextArray
            -> ChanOutput PlaintextArray
            -> Tower e ()
canDatalink tx rx assembled toFrag = do
  fragmentReceiver rx [fragmentReceiveHandler assembled s2cType]
  fragmentSenderBlind toFrag c2sType tx


datalinkEncode :: (e -> DatalinkMode)
               -> ChanOutput PlaintextArray
               -> ChanInput CyphertextArray
               -> Tower e ()
datalinkEncode todm pt ct = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> padTower' pt ct
    SymmetricCommsecMode DatalinkServer sk ->
      commsecEncodeTower' "dl" (symKeySaltArrayIval (sk_s2c sk)) pt ct
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxy.datalinkEncode: "
                  ++ "unsupported datalink mode " ++ show datalinkMode )
datalinkDecode :: (e -> DatalinkMode)
               -> ChanOutput CyphertextArray
               -> ChanInput PlaintextArray
               -> Tower e ()
datalinkDecode todm ct pt = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> unpadTower' ct pt
    SymmetricCommsecMode DatalinkServer sk ->
      commsecDecodeTower' "dl" (symKeySaltArrayIval (sk_c2s sk)) ct pt
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxy.datalinkDecode: "
                  ++ "unsupported datalink mode " ++ show datalinkMode )

uartDatalink :: (e -> ClockConfig)
             -> UART_Device
             -> Integer
             -> ChanInput CyphertextArray
             -> ChanOutput CyphertextArray
             -> Tower e ()
uartDatalink tocc uart baud input output = do
  (uarto, uarti) <- uartTower tocc
                              (uart_periph uart)
                              (uart_pins   uart)
                              baud

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

