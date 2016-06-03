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
import SMACCMPilot.Flight.Datalink.UART
import SMACCMPilot.Flight.Datalink.Commsec
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Commsec.Sizes

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

  commsecEncodeDatalink todl c2s_pt_to_uart (fst c2s_ct_to_uart)
  commsecDecodeDatalink todl (snd s2c_ct_from_uart) (fst s2c_pt_from_uart)

  mon <- uartDatalink tocc (fp_telem fp) 115200 (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)
  monitor "uart_dma" mon
  canDatalink canTx canRx (fst c2s_from_can) s2c_to_can

  return ()
  where
  tocc = fp_clockconfig . tofp
  todl = fp_datalink . tofp

canDatalink :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
            -> ChanOutput ('Struct "can_message")
            -> ChanInput PlaintextArray
            -> ChanOutput PlaintextArray
            -> Tower e ()
canDatalink tx rx assembled toFrag = do
  fragmentReceiver rx [fragmentReceiveHandler assembled s2cType]
  fragmentSenderBlind toFrag c2sType tx


