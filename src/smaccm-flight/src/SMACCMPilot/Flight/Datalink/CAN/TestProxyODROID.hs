{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID
  ( app
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface

import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
import SMACCMPilot.Flight.Datalink.Commsec (padTower', unpadTower')
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Ivory.Types.SequenceNumberedCameraTarget
import SMACCMPilot.Comm.Ivory.Types.CameraTarget
import SMACCMPilot.Comm.Ivory.Types.SequenceNum
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Tower
import SMACCMPilot.Datalink.Mode

import SMACCMPilot.Datalink.HXStream.Tower

import Tower.Odroid.CAN
import Tower.Odroid.UART
import Tower.Odroid.CameraVM

app :: (e -> DatalinkMode)
    -> Maybe (ChanOutput (Struct "bbox"))
    -> Tower e ()
app todl mbboxRx = do
  towerDepends stdIOModule
  towerModule  stdIOModule

  (canRx, canTx) <- canTower

  s2c_pt_from_uart <- channel
  s2c_ct_from_uart <- channel
  c2s_from_can     <- channel

  camera_tgt_req   <- channel

  cv_producer      <- controllableVehicleProducerInput (snd c2s_from_can)
  c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer
  cv_consumer      <- controllableVehicleConsumerInput (snd s2c_pt_from_uart)
  let cv_consumer' = cv_consumer { cameraTargetInputSetReqConsumer = snd camera_tgt_req
                                 }
  s2c_to_can      <- controllableVehicleConsumerOutput cv_consumer'

  c2s_ct_to_uart  <- channel

  clk             <- period (1000`ms`)

  datalinkEncode todl c2s_pt_to_uart (fst c2s_ct_to_uart)
  datalinkDecode todl (snd s2c_ct_from_uart) (fst s2c_pt_from_uart)

  uartDatalink (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)
  canDatalink canTx canRx (fst c2s_from_can) s2c_to_can

  case mbboxRx of
    Nothing
      -> return ()
    Just bboxRx
      -> do
        monitor "periodic_camera_injector" $ do
           bbox_st   <- stateInit "camera_toggle" izero
           bbox_prev <- stateInit "camera_toggle" izero
           set_req   <- stateInit "camera_req"    izero

           handler clk "camera_clk" $ do
             e_set <- emitter (fst camera_tgt_req) 1
             callback $ const $ do
               comment "Made up for now"
               l  <- deref (bbox_st ~> left)
               r  <- deref (bbox_st ~> right)

               l' <- deref (bbox_prev ~> left)
               r' <- deref (bbox_prev ~> right)

               unless (l ==? l' .&& r ==? r') $ do
                 refCopy bbox_prev bbox_st
                 (set_req ~> seqnum) += 1
                 snum <- deref (set_req~>seqnum)
                 call_ printfuint32 "**** SENDING BBOX with seq %u\n" (unSequenceNum snum)
                 let ct = set_req ~> val
                 store (ct ~> valid)       true
                 store (ct ~> angle_up)    (safeCast l)
                 store (ct ~> angle_right) (safeCast r)
                 emit e_set (constRef set_req)

           handler bboxRx "bboxRx" $ do
             callback $ \bbox -> do
               refCopy bbox_st bbox

           handler (cameraTargetInputSetRespProducer cv_producer) "set_response" $ do
             callback $ \seqNum -> do
               comment "camera target setting has been acknowledged"
               s <- deref seqNum
               call_ printfuint32 "**** SeqNum ack: %u\n" (unSequenceNum s)

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
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID.datalinkEncode: "
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
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID.datalinkDecode: "
                  ++ "unsupported datalink mode " ++ show datalinkMode )

uartDatalink :: ChanInput CyphertextArray
             -> ChanOutput CyphertextArray
             -> Tower e ()
uartDatalink input output = do
  (uarto, uarti) <- uartTower

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


----------------------------------------
--- XXX should be put into a separate lib at some point.

[ivory|
import (stdio.h, printf) void printfuint32(string x, uint32_t y)
import (stdio.h, printf) void printf(string x)
|]


stdIOModule :: Module
stdIOModule = package "stdIOModule" $ do
  incl printfuint32
  incl printf
