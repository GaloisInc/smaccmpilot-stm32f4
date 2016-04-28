{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID
  ( app
  ) where

import Data.List (foldl')

import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Ivory.Tower

import Tower.Mini.Component

import Ivory.Tower.HAL.Bus.CAN.Fragment (
    fragmentReceiveHandler
  , fragmentReceiver
  , fragmentSender
  , MessageType
  )

import Ivory.Tower.HAL.Bus.Interface (AbortableTransmit(..), BackpressureTransmit(..))

import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
import SMACCMPilot.Flight.Datalink.Commsec (
    commsecEncodeDatalink
  , commsecDecodeDatalink)
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Ivory.Types.SequenceNum (SequenceNum)
import SMACCMPilot.Comm.Ivory.Types.SequenceNumberedCameraTarget (seqnum, val)
import SMACCMPilot.Comm.Ivory.Types.CameraTarget (
    bbox_l
  , bbox_r
  , bbox_t
  , bbox_b
  , valid
  )
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle (
    cameraTargetInputGetReqConsumer
  , cameraTargetInputGetRespProducer
  , controllableVehicleConsumerInput
  , controllableVehicleConsumerOutput
  , controllableVehicleProducerInput
  , controllableVehicleProducerOutput
  )
import SMACCMPilot.Commsec.Sizes (PlaintextArray)
import SMACCMPilot.Datalink.Mode (DatalinkMode)

import SMACCMPilot.Datalink.HXStream.Tower (
    airDataEncodeTower
  , airDataDecodeTower
  )

import Tower.Odroid.CAN (canTower)
import Tower.Odroid.UART (uartTower, UartPacket)
import qualified Tower.Odroid.CameraVM as VM

app :: (e -> DatalinkMode)
    -> Tower e (Maybe (ChanOutput ('Struct "camera_data")))
    -> Tower e ()
app todl mkCameraVM = do
  -- CameraVM component
  cameraRx <- mkCameraVM
  case cameraRx of
    Nothing -> return ()
    Just rx -> component "Camera_VM" $ do
      outputPort' rx "put_CameraVM_data" "CameraVM_Server_intermon.h"

  component "Server" $ do
    s2c_pt_from_uart <- inputPort "get_UART_in_data" "UART_in_Server_intermon.h"
    c2s_from_can     <- inputPort "get_Server_in_data"  "Server_CAN_intermon.h"
    camera_data_chan <- inputPort "get_CameraVM_data" "CameraVM_Server_intermon.h"

    (c2s_pt_to_uart, s2c_to_can) <- liftTower $ do
      camera_chan      <- channel
      cv_producer      <- controllableVehicleProducerInput (c2s_from_can :: ChanOutput PlaintextArray)
      let cv_producer' =  cv_producer { cameraTargetInputGetRespProducer
                                      = snd camera_chan }
      c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer'
      cv_consumer      <- controllableVehicleConsumerInput (s2c_pt_from_uart :: ChanOutput PlaintextArray)
      s2c_to_can       <- controllableVehicleConsumerOutput cv_consumer
      case cameraRx of
        Nothing -> return ()
        Just _ -> do
          periodicCamera (fst camera_chan) camera_data_chan (cameraTargetInputGetReqConsumer cv_consumer)
      return ( c2s_pt_to_uart :: ChanOutput PlaintextArray
             , s2c_to_can     :: ChanOutput PlaintextArray
             )

    outputPort' c2s_pt_to_uart "put_UART_out_data" "UART_out_Server_intermon.h"
    outputPort' s2c_to_can "put_Server_out_data" "Server_CAN_intermon.h"

  component "CAN" $ do
    s2c_to_can <- inputPort "get_Server_out_data" "Server_CAN_intermon.h"
    canRx <- inputPort "get_CAN_hw_recv_data" "CAN_CAN_hw_intermon.h"
    status <- inputPort "get_CAN_hw_status_data" "CAN_CAN_hw_intermon.h"
    send <- outputPort "put_CAN_hw_send_data" "CAN_CAN_hw_intermon.h"
    abort <- outputPort "put_CAN_hw_abort_data" "CAN_CAN_hw_intermon.h"
    c2s_from_can <- outputPort "put_Server_in_data" "Server_CAN_intermon.h"
    let canTx = AbortableTransmit send abort status
    liftTower $ canDatalink canTx canRx c2s_from_can s2c_to_can

  component "CAN_hw" $ do
    (canRx, AbortableTransmit send abort status) <- liftTower canTower
    inputPort' send "get_CAN_hw_send_data" "CAN_CAN_hw_intermon.h"
    inputPort' abort "get_CAN_hw_abort_data" "CAN_CAN_hw_intermon.h"
    outputPort' status "put_CAN_hw_status_data" "CAN_CAN_hw_intermon.h"
    outputPort' canRx "put_CAN_hw_recv_data" "CAN_CAN_hw_intermon.h"

  uartInComponent  todl
  uartOutComponent todl
  uartHwComponent

periodicCamera :: ChanInput ('Struct "sequence_numbered_camera_target")
               -> ChanOutput ('Struct "camera_data")
               -> ChanOutput ('Stored SequenceNum)
               -> Tower e ()
periodicCamera camera_chan_tx camera_data_chan camera_req_tx = do

  monitor "periodic_camera_injector" $ do
     camera_data_st    <- stateInit "camera_data_st"    izero
     camera_data_prev  <- stateInit "camera_data_prev"  izero
     camera_data_watch <- stateInit "camera_data_watch" (izero :: Init ('Stored Uint32))

     handler camera_req_tx "camera_req" $ do
       e <- emitter camera_chan_tx 1
       callback $ \curr_seq -> do

         l  <- deref (camera_data_st ~> VM.bbox_l)
         r  <- deref (camera_data_st ~> VM.bbox_r)
         t  <- deref (camera_data_st ~> VM.bbox_t)
         b  <- deref (camera_data_st ~> VM.bbox_b)

         l' <- deref (camera_data_prev ~> VM.bbox_l)
         r' <- deref (camera_data_prev ~> VM.bbox_r)
         t' <- deref (camera_data_prev ~> VM.bbox_t)
         b' <- deref (camera_data_prev ~> VM.bbox_b)

         comment "bbox inside bounds and invariants hold on l,r & t,b"
         let valid_bounds = r <=? 320 .&& l <? r .&& b <=? 200 .&& t <? b
         comment "at least one element is nonzero"
         let nzero bool v = bool .|| (v /=? 0)
         comment "at least one bound has changed"
         ifte_ (l /=? l' .|| r /=? r' .|| t /=? t' .|| b /=? b')
               (camera_data_watch %= const 0)
               (camera_data_watch += 1)

         s <- deref curr_seq
         set_req <- local izero
         store (set_req ~> seqnum) s
         let ct = set_req ~> val

         cdw <- deref camera_data_watch
         comment "Valid packet: valid bounds, at least one nonzero element, and no more at 10 requests without a corner changing."
         comment "Yes, I know the 2nd property could be make to be a consequence of the 3rd."
         let isValid = foldl' nzero false [l,r,t,b]
                   .&& valid_bounds
                   .&& cdw <? 10
         store (ct ~> valid) isValid

         store (ct ~> bbox_l) l
         store (ct ~> bbox_r) r
         store (ct ~> bbox_t) t
         store (ct ~> bbox_b) b
         refCopy camera_data_prev camera_data_st
         emit e (constRef set_req)

     handler camera_data_chan "cameraDataRx" $ do
       callback $ \camera_data -> do
         refCopy camera_data_st camera_data

fragmentDrop :: (IvoryArea a, IvoryZero a)
                    => ChanOutput a
                    -> MessageType a
                    -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
                    -> Tower e ()
fragmentDrop src mt tx = do
  (fragReq, _fragAbort, fragDone) <- fragmentSender mt tx

  monitor "fragment_drop" $ do
    msg          <- state "can_msg"
    in_progress  <- stateInit "in_progress" (ival false)

    handler src "new_msg" $ do
      toFrag <- emitter fragReq 1
      callback $ \ new_msg -> do
        was_in_progress <- deref in_progress
        unless was_in_progress $ do
          refCopy msg new_msg
          emit toFrag (constRef msg)
          store in_progress true

    handler fragDone "fragment_done" $ do
      callback $ const $ store in_progress false

canDatalink :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
            -> ChanOutput ('Struct "can_message")
            -> ChanInput PlaintextArray
            -> ChanOutput PlaintextArray
            -> Tower e ()
canDatalink tx rx assembled toFrag = do
  fragmentReceiver rx [fragmentReceiveHandler assembled s2cType]
  fragmentDrop toFrag c2sType tx

uartInComponent :: (e -> DatalinkMode) -> Tower e ()
uartInComponent todl = component "UART_in" $ do
  uarti <- inputPort "get_UART_hw_packet_in_data" "UART_hw_UART_in_intermon.h"
  input <- outputPort "put_UART_in_data" "UART_in_Server_intermon.h"
  liftTower $ do
    input_frames <- channel
    decoderChan  <- channel
    s2c_ct_from_uart <- channel

    monitor "to_hx" $ do
      handler (uarti :: ChanOutput UartPacket) "unpack" $ do
        let n = fromTypeNat (aNat :: Proxy (Capacity UartPacket))
        e <- emitter (fst decoderChan) n
        callback $ \msg -> do
          len <- msg ~>* stringLengthL
          let d = msg ~> stringDataL
          arrayMap $ \ix -> do
            when (fromIx ix <? len) $ emit e (d!ix)

    airDataDecodeTower "frame" (snd decoderChan) (fst input_frames)

    frameBuffer' (snd input_frames) (Milliseconds 5) (Proxy :: Proxy 4) (fst s2c_ct_from_uart)

    commsecDecodeDatalink todl (snd s2c_ct_from_uart) input


uartOutComponent :: (e -> DatalinkMode) -> Tower e ()
uartOutComponent todl = component "UART_out" $ do
  output <- inputPort "get_UART_out_data" "UART_out_Server_intermon.h"
  status <- inputPort "get_UART_hw_status_data" "UART_hw_UART_out_intermon.h"
  packet <- outputPort "put_UART_hw_packet_out_data" "UART_hw_UART_out_intermon.h"
  let uarto = BackpressureTransmit packet (status :: ChanOutput ('Stored IBool))
  liftTower $ do
    c2s_ct_to_uart <- channel
    airDataEncodeTower "frame" (snd c2s_ct_to_uart) uarto
    commsecEncodeDatalink todl output (fst c2s_ct_to_uart)

uartHwComponent :: Tower e ()
uartHwComponent = component "UART_hw" $ do
  (BackpressureTransmit output status, input) <- liftTower uartTower
  inputPort' (output :: ChanInput UartPacket) "get_UART_hw_packet_out_data" "UART_hw_UART_out_intermon.h"
  outputPort' status "put_UART_hw_status_data" "UART_hw_UART_out_intermon.h"
  outputPort' (input :: ChanOutput UartPacket) "put_UART_hw_packet_in_data" "UART_hw_UART_in_intermon.h"
