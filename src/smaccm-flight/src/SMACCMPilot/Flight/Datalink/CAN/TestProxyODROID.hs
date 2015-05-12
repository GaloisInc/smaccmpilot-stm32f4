{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID
  ( app
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface

import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
import SMACCMPilot.Flight.Datalink.Commsec (padTower', unpadTower')
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Tower
import SMACCMPilot.Datalink.Mode

import SMACCMPilot.Datalink.HXStream.Tower

import Tower.Odroid.CAN
import Tower.Odroid.UART


app :: Tower e ()
app = do
  (canRx, canTx) <- canTower

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

  uartDatalink (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)
  canDatalink canTx canRx (fst c2s_from_can) s2c_to_can

  where
  todl _ = PlaintextMode

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
    SymmetricCommsecMode sk ->
      commsecEncodeTower' "dl" (symKeySaltArrayIval (sk_s2c sk)) pt ct

datalinkDecode :: (e -> DatalinkMode)
               -> ChanOutput CyphertextArray
               -> ChanInput PlaintextArray
               -> Tower e ()
datalinkDecode todm ct pt = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> unpadTower' ct pt
    SymmetricCommsecMode sk ->
      commsecDecodeTower' "dl" (symKeySaltArrayIval (sk_c2s sk)) ct pt

uartDatalink :: ChanInput CyphertextArray
             -> ChanOutput CyphertextArray
             -> Tower e ()
uartDatalink input output = do
  (uarto, uarti) <- uartTower

  input_frames <- channel

  hxstreamDecodeTower "frame" uarti (fst input_frames)

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

  hxstreamEncodeTower "frame" output uarto

