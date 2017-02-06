{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Data.Foldable (for_)
import Data.List (foldl')

import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.Config

import Tower.Mini

import Ivory.Tower.HAL.Bus.CAN.Fragment (
    fragmentReceiveHandler
  , fragmentReceiver
  , fragmentSender
  , MessageType
  )

import Ivory.Tower.HAL.Bus.Interface (
    AbortableTransmit(..)
  , BackpressureTransmit(..)
  )

import Ivory.Tower.HAL.RingBuffer (bufferChans)

import SMACCMPilot.Flight.Datalink.Commsec (
    commsecEncodeDatalink
  , commsecDecodeDatalink)
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Ivory.Types.SequenceNum (SequenceNum)

import SMACCMPilot.Comm.Ivory.Types.SequenceNumberedCameraTarget (
    seqnum
  , val
  )
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
  , rebootReqSetReqConsumer
  )
import SMACCMPilot.Commsec.Sizes (PlaintextArray)
import SMACCMPilot.Datalink.Mode (DatalinkMode)

import SMACCMPilot.Datalink.Mode (datalinkModeParser, DatalinkRole(..))

import SMACCMPilot.Datalink.HXStream.Tower (
    airDataEncodeTower
  , airDataDecodeTower
  )

import Tower.Odroid.UART (UartPacket)
import qualified Tower.Odroid.CameraVM as VM

import qualified SMACCMTypes.CAN        as SMACCM_CAN
import qualified SMACCMTypes.Camera     as SMACCM_Camera
import qualified SMACCMTypes.UARTPacket as SMACCM_UART

server, can, decrypt, encrypt :: String
server   = "Server"
can      = "CAN_Framing"
decrypt  = "Decrypt"
encrypt  = "Encrypt"

app :: (e -> DatalinkMode) -> Bool -> [Component e]
app todl cameraPresent =
  let canHdr     = "tb_CAN_Framing.h"
      decryptHdr = "tb_Decrypt.h"
      encryptHdr = "tb_Encrypt.h"
      serverHdr  = "tb_Server.h"
      cameraVm2server =
        mkExternalInputChan "tb_vm2self_read" serverHdr
      server2cameraVm =
        mkExternalOutputChan "tb_self2vm_reboot_enqueue" serverHdr
      uartHw2decrypt =
        mkExternalInputChan "tb_uart2self_dequeue" decryptHdr
      uartHw2encrypt =
        mkExternalInputChan "tb_uart2self_dequeue" encryptHdr
      decrypt2server =
        mkExternalChan
          "tb_decrypt2self_dequeue" serverHdr
          "tb_self2server_enqueue" decryptHdr
      encrypt2uartHw =
        mkExternalOutputChan "tb_self2uart_enqueue" encryptHdr
      server2encrypt =
        mkExternalChan
          "tb_server2self_dequeue" encryptHdr
          "tb_self2encrypt_enqueue" serverHdr
      server2can =
        mkExternalChan
          "tb_server2self_dequeue" canHdr
          "tb_self2framing_enqueue" serverHdr
      can2server =
        mkExternalChan
          "tb_framing2self_dequeue" serverHdr
          "tb_self2server_enqueue" canHdr
      can2canHw_send =
        mkExternalOutputChan "tb_self2can_enqueue" canHdr
      canHw2can_recv =
        mkExternalInputChan "tb_can2self_frame_dequeue" canHdr
      canHw2can_status =
        mkExternalInputChan "tb_can2self_status_dequeue" canHdr
  in [
    serverComponent cameraPresent
      decrypt2server
      can2server
      cameraVm2server
      server2cameraVm
      server2encrypt
      server2can
  , canComponent
      server2can
      can2server
      canHw2can_recv
      canHw2can_status
      can2canHw_send
  , decryptComponent todl
      uartHw2decrypt
      decrypt2server
  , encryptComponent todl
      encrypt2uartHw
      uartHw2encrypt
      server2encrypt
  ]

serverComponent
  :: Bool
  -> ExternalChan PlaintextArray
  -> ExternalChan PlaintextArray
  -> ExternalInputChan ('Struct "SMACCM_DATA__Camera_Bounding_Box_i")
  -> ExternalOutputChan ('Stored IBool)
  -> ExternalChan PlaintextArray
  -> ExternalChan PlaintextArray
  -> Component e
serverComponent cameraPresent
  decrypt2server
  can2server
  cameraVm2server
  server2cameraVm
  server2encrypt
  server2can
  =
  component server $ do
    s2c_pt_from_uart <- inputPortChan decrypt2server
    c2s_from_can     <- inputPortChan can2server
    camera_data_in   <- inputPortChan cameraVm2server

    (c2s_pt_to_uart, s2c_to_can, reboot_req_to_vm) <- tower $ do
      camera_chan      <- channel
      cv_producer      <- controllableVehicleProducerInput c2s_from_can
      let cv_producer' =  cv_producer { cameraTargetInputGetRespProducer
                                      = snd camera_chan }
      c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer'
      cv_consumer      <- controllableVehicleConsumerInput s2c_pt_from_uart
      s2c_to_can       <- controllableVehicleConsumerOutput cv_consumer
      reboot_req_to_vm <-
        case cameraPresent of
          False -> return Nothing
          True -> do
            periodicCamera
              (fst camera_chan)
              camera_data_in
              (cameraTargetInputGetReqConsumer cv_consumer)

            reboot_req <- channel
            monitor "reboot_req_unwrapper" $ do
              handler (rebootReqSetReqConsumer cv_consumer) "reboot_vm" $ do
                e <- emitter (fst reboot_req) 1
                callback $ \_ -> do
                  emitV e true
            return (Just (snd reboot_req))

      return (c2s_pt_to_uart, s2c_to_can, reboot_req_to_vm)
    outputPortChan' c2s_pt_to_uart server2encrypt
    outputPortChan' s2c_to_can server2can
    for_ reboot_req_to_vm $ \chan ->
      outputPortChan' chan server2cameraVm

periodicCamera :: ChanInput ('Struct "sequence_numbered_camera_target")
               -> ChanOutput ('Struct "SMACCM_DATA__Camera_Bounding_Box_i")
               -> ChanOutput ('Stored SequenceNum)
               -> Tower e ()
periodicCamera camera_chan_tx smaccm_camera_data_chan camera_req_tx = do
  camera_data_chan <- SMACCM_Camera.from_smaccm smaccm_camera_data_chan

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
         let nzero bit v = bit .|| (v /=? 0)
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

canComponent :: ExternalChan PlaintextArray
             -> ExternalChan PlaintextArray
             -> ExternalInputChan ('Struct "SMACCM_DATA__CAN_Frame_i")
             -> ExternalInputChan ('Stored IBool)
             -> ExternalOutputChan ('Struct "SMACCM_DATA__CAN_Frame_i")
             -> Component e
canComponent
  server2can
  can2server
  canHw2can_recv
  canHw2can_status
  can2canHw_send
  =
  component can $ do
    s2c_to_can   <- inputPortChan  server2can
    smaccm_canRx <- inputPortChan  canHw2can_recv
    status       <- inputPortChan  canHw2can_status
    smaccm_send  <- outputPortChan can2canHw_send
    c2s_from_can <- outputPortChan can2server
    tower $ do
      -- unused for the Ph3 implementation
      (_abort, _) <- channel
      send <- SMACCM_CAN.to_smaccm smaccm_send
      let canTx = AbortableTransmit send _abort status
      canRx <- SMACCM_CAN.from_smaccm smaccm_canRx
      canDatalink canTx canRx c2s_from_can s2c_to_can

decryptComponent :: (e -> DatalinkMode)
                 -> ExternalInputChan ('Struct "SMACCM_DATA__UART_Packet_i")
                 -> ExternalChan PlaintextArray
                 -> Component e
decryptComponent todl uart_in_ext pt_out_ext =
  component decrypt $ do
    uarti <- inputPortChan uart_in_ext
    input <- outputPortChan pt_out_ext
    tower $ do
      input_frames <- channel
      decoderChan  <- channel
      s2c_ct_from_uart <- channel
      uarti' <- SMACCM_UART.from_smaccm uarti

      monitor "to_hx" $ do
        handler uarti' "unpack" $ do
          let n = fromTypeNat (aNat :: Proxy (Capacity UartPacket))
          e <- emitter (fst decoderChan) n
          callback $ \msg -> do
            len <- msg ~>* stringLengthL
            let d = msg ~> stringDataL
            arrayMap $ \ix -> do
              when (fromIx ix <? len) $ emit e (d!ix)

      airDataDecodeTower "frame" (snd decoderChan) (fst input_frames)

      bufferChans
        (snd input_frames)
        (Milliseconds 5)
        (Proxy :: Proxy 4)
        (fst s2c_ct_from_uart)

      commsecDecodeDatalink todl (snd s2c_ct_from_uart) input

encryptComponent :: (e -> DatalinkMode)
                 -> ExternalOutputChan ('Struct "SMACCM_DATA__UART_Packet_i")
                 -> ExternalInputChan ('Stored IBool)
                 -> ExternalChan PlaintextArray
                 -> Component e
encryptComponent todl encrypt2uartHw uartHw2encrypt server2encrypt =
  component encrypt $ do
    output <- inputPortChan server2encrypt
    status <- inputPortChan uartHw2encrypt
    smaccm_packet <- outputPortChan encrypt2uartHw
    tower $ do
      c2s_ct_to_uart <- channel
      commsecEncodeDatalink todl output (fst c2s_ct_to_uart)

      (hx_packet_in, hx_packet_out) <- channel
      let uarto = BackpressureTransmit hx_packet_in status
      airDataEncodeTower
        "frame"
        (snd c2s_ct_to_uart)
        uarto
        (Milliseconds 5)
        (Proxy :: Proxy 4)

      packet <- SMACCM_UART.to_smaccm smaccm_packet
      monitor "send_transdata" $ do
        handler hx_packet_out "send_translate" $ do
          e <- emitter packet 1
          callback $ \msg -> do
            msg' <- local (izero :: Init UartPacket)
            let srccap = arrayLen (msg ~> stringDataL)
            srclen <- msg ~>* stringLengthL
            assert $ srclen >=? 0 .&& srclen <=? srccap
            assert $ srccap <=? arrayLen (msg' ~> stringDataL)
            arrayCopy (msg' ~> stringDataL) (msg ~> stringDataL) 0 srclen
            store (msg' ~> stringLengthL) srclen
            emit e $ constRef msg'

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

main :: IO ()
main = do
  compileTowerMini fst p (app snd True)
  where
  p topts = fmap fst $ getConfig' topts $ do
    k <- datalinkModeParser DatalinkServer
    return (defaultMiniConfig,k)
