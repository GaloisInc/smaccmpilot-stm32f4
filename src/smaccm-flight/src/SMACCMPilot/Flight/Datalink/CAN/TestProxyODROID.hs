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

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.RingBuffer

import SMACCMPilot.Flight.Datalink.Commsec
import SMACCMPilot.Flight.Datalink.CAN (s2cType, c2sType)

import SMACCMPilot.Comm.Ivory.Types.SequenceNum
import SMACCMPilot.Comm.Ivory.Types.SequenceNumberedCameraTarget
import SMACCMPilot.Comm.Ivory.Types.CameraTarget
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.Mode

import SMACCMPilot.Datalink.HXStream.Tower

import Tower.Odroid.CAN
import Tower.Odroid.UART
import qualified Tower.Odroid.CameraVM as VM

type CameraSetup e = ChanOutput ('Struct "sequence_numbered_reboot_req")
                  -> Tower e (ChanOutput ('Struct "camera_data"))

app :: (e -> DatalinkMode)
    -> Maybe (CameraSetup e)
    -> Tower e ()
app todl mCameraSetup = do

  (canRx, canTx) <- canTower

  s2c_pt_from_uart <- channel
  s2c_ct_from_uart <- channel
  c2s_from_can     <- channel

  camera_chan        <- channel

  cv_producer      <- controllableVehicleProducerInput (snd c2s_from_can)
  let cv_producer' =  cv_producer { cameraTargetInputGetRespProducer = snd camera_chan }
  c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer'

  cv_consumer      <- controllableVehicleConsumerInput (snd s2c_pt_from_uart)
  s2c_to_can       <- controllableVehicleConsumerOutput cv_consumer

  c2s_ct_to_uart  <- channel
  return ()

  commsecEncodeDatalink todl c2s_pt_to_uart (fst c2s_ct_to_uart)
  commsecDecodeDatalink todl (snd s2c_ct_from_uart) (fst s2c_pt_from_uart)

  uartDatalink (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)

  canDatalink  canTx canRx (fst c2s_from_can) s2c_to_can

  case mCameraSetup of
    Nothing -> return ()
    Just cameraSetup -> do
      camera_data_chan <- cameraSetup (rebootReqSetReqConsumer cv_consumer)
      periodicCamera (fst camera_chan) camera_data_chan (cameraTargetInputGetReqConsumer cv_consumer)

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

uartDatalink :: ChanInput CyphertextArray
             -> ChanOutput CyphertextArray
             -> Tower e ()
uartDatalink input output = do
  (BackpressureTransmit uarto status, uarti) <- uartTower

  input_frames <- channel
  decoderChan  <- channel

  monitor "to_hx" $ do
    handler uarti "unpack" $ do
      let n = fromTypeNat (aNat :: Proxy (Capacity UartPacket))
      e <- emitter (fst decoderChan) n
      callback $ \msg -> do
        len <- msg ~>* stringLengthL
        let d = msg ~> stringDataL
        arrayMap $ \ix -> do
          when (fromIx ix <? len) $ emit e (d!ix)

  (hx_packet_in, hx_packet_out) <- channel
  let translate = BackpressureTransmit hx_packet_in status

  monitor "send_transdata" $ do
    handler hx_packet_out "send_translate" $ do
      e <- emitter uarto 1
      callback $ \msg -> do
        msg' <- local (izero :: Init UartPacket)
        let srccap = arrayLen (msg ~> stringDataL)
        srclen <- msg ~>* stringLengthL
        assert $ srclen >=? 0 .&& srclen <=? srccap
        assert $ srccap <=? arrayLen (msg' ~> stringDataL)
        arrayCopy (msg' ~> stringDataL) (msg ~> stringDataL) 0 srclen
        store (msg' ~> stringLengthL) srclen
        emit e $ constRef msg'


  airDataDecodeTower "frame" (snd decoderChan) (fst input_frames)

  bufferChans (snd input_frames) (Milliseconds 5) (Proxy :: Proxy 4) input

  airDataEncodeTower "frame" output translate (Milliseconds 5) (Proxy :: Proxy 4)
