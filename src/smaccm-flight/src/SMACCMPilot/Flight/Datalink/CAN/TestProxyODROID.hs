{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID
  ( app
  ) where

import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Ivory.Tower

import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface

import SMACCMPilot.Flight.Datalink.UART (frameBuffer')
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

app :: (e -> DatalinkMode)
    -> Maybe (ChanOutput (Struct "camera_data"))
    -> Tower e ()
app todl cameraRx = do

  (canRx, canTx) <- canTower

  s2c_pt_from_uart <- channel
  s2c_ct_from_uart <- channel
  c2s_from_can     <- channel

  camera_chan      <- channel

  cv_producer      <- controllableVehicleProducerInput (snd c2s_from_can)
  let cv_producer' =  cv_producer { cameraTargetInputGetRespProducer
                                  = snd camera_chan }
  c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer'

  cv_consumer      <- controllableVehicleConsumerInput (snd s2c_pt_from_uart)
  -- cv_consumer'     <- cv_consumer { cameraTargetInputGetReqConsumer
  --                                 = snd camera_chan }
  s2c_to_can       <- controllableVehicleConsumerOutput cv_consumer

  c2s_ct_to_uart  <- channel
  return ()

  commsecEncodeDatalink todl c2s_pt_to_uart (fst c2s_ct_to_uart)
  commsecDecodeDatalink todl (snd s2c_ct_from_uart) (fst s2c_pt_from_uart)

  uartDatalink (fst s2c_ct_from_uart) (snd c2s_ct_to_uart)

  canDatalink  canTx canRx (fst c2s_from_can) s2c_to_can

  case cameraRx of
    Nothing
      -> return ()
    Just camera_data_chan
      -> periodicCamera (fst camera_chan) camera_data_chan (cameraTargetInputGetReqConsumer cv_consumer)

periodicCamera :: ChanInput (Struct "sequence_numbered_camera_target")
               -> ChanOutput (Struct "camera_data")
               -> ChanOutput (Stored SequenceNum)
               -> Tower e ()
periodicCamera camera_chan_tx camera_data_chan camera_req_tx = do

  monitor "periodic_camera_injector" $ do
     camera_data_st   <- stateInit "camera_data_st"   izero

     handler camera_req_tx "camera_req" $ do
       e <- emitter camera_chan_tx 1
       callback $ \curr_seq -> do

         l  <- deref (camera_data_st ~> VM.bbox_l)
         r  <- deref (camera_data_st ~> VM.bbox_r)
         t  <- deref (camera_data_st ~> VM.bbox_t)
         b  <- deref (camera_data_st ~> VM.bbox_b)

         s <- deref curr_seq
         set_req <- local izero
         store (set_req ~> seqnum) s
         let ct = set_req ~> val
         store (ct ~> valid)  true
         store (ct ~> bbox_l) l
         store (ct ~> bbox_r) r
         store (ct ~> bbox_t) t
         store (ct ~> bbox_b) b
         emit e (constRef set_req)

     handler camera_data_chan "cameraDataRx" $ do
       callback $ \camera_data -> do
         refCopy camera_data_st camera_data

fragmentDrop :: (IvoryArea a, IvoryZero a)
                    => ChanOutput a
                    -> MessageType a
                    -> AbortableTransmit (Struct "can_message") (Stored IBool)
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

canDatalink :: AbortableTransmit (Struct "can_message") (Stored IBool)
            -> ChanOutput (Struct "can_message")
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
  (uarto, uarti) <- uartTower

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

  airDataDecodeTower "frame" (snd decoderChan) (fst input_frames)

  frameBuffer' (snd input_frames) (Milliseconds 5) (Proxy :: Proxy 4) input

  airDataEncodeTower "frame" output uarto

