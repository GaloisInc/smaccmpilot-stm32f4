{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module SMACCMPilot.Flight.Datalink.CAN.TestProxyODROID
  ( app
  ) where

import Data.List (foldl', intercalate)

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
  , HXCyphertext
  )

import Tower.Odroid.CAN (canTower)
import Tower.Odroid.UART (uartTower, UartPacket)
import qualified Tower.Odroid.CameraVM as VM

cameraVm, server, can, canHw, uartIn, uartOut, uartHw :: String
cameraVm = "Camera_VM"
server   = "Server"
can      = "CAN"
canHw    = "CAN_hw"
uartIn   = "UART_in"
uartOut  = "UART_out"
uartHw   = "UART_hw"

-- | Make an 'ExternalChan' between components @c1@ and @c2@ with a
-- name to disambiguate multiple operations in the same intermediate
-- monitor
mkChan :: String -> String -> String -> ExternalChan a
mkChan c1 c2 nm =
  ExternalChan
    (intercalate "_" [c1, c2, "get", nm])
    (intercalate "_" [c1, c2, "put", nm])
    (intercalate "_" [c1, c2, "intermon"] ++ ".h")

app :: (e -> DatalinkMode)
    -> Maybe (Tower e (ChanOutput ('Struct "camera_data")))
    -> [Component e]
app todl mkCameraVm =
  let cameraVm2server  = mkChan cameraVm server  "camera_data"
      uartHw2uartIn    = mkChan uartHw   uartIn  "packet"
      uartHw2uartOut   = mkChan uartHw   uartOut "bool"
      uartIn2server    = mkChan uartIn   server  "pt_data"
      uartOut2uartHw   = mkChan uartOut  uartHw  "packet"
      server2uartOut   = mkChan server   uartOut "pt_data"
      server2can       = mkChan server   can     "pt_data"
      can2server       = mkChan can      server  "pt_data"
      can2canHw_send   = mkChan can      canHw   "can_message"
      can2canHw_abort  = mkChan can      canHw   "abort"
      canHw2can_recv   = mkChan canHw    can     "can_message"
      canHw2can_status = mkChan canHw    can     "status"
  in [
    cameraVmComponent mkCameraVm cameraVm2server
  , serverComponent mkCameraVm uartIn2server can2server cameraVm2server server2uartOut server2can
  , canComponent server2can can2server canHw2can_recv canHw2can_status can2canHw_send can2canHw_abort
  , canHwComponent can2canHw_send can2canHw_abort canHw2can_status canHw2can_recv
  , uartInComponent todl uartHw2uartIn uartIn2server
  , uartOutComponent todl uartOut2uartHw uartHw2uartOut server2uartOut
  , uartHwComponent uartOut2uartHw uartHw2uartIn uartHw2uartOut
  ]

cameraVmComponent :: Maybe (Tower e (ChanOutput ('Struct "camera_data")))
                  -> ExternalChan ('Struct "camera_data")
                  -> Component e
cameraVmComponent mkCameraVm cameraVm2server =
  component cameraVm $ do
    case mkCameraVm of
      Nothing -> return ()
      Just mk -> do
        rx <- liftTower mk
        outputPortChan' rx cameraVm2server

serverComponent :: Maybe (Tower e (ChanOutput ('Struct "camera_data")))
                -> ExternalChan PlaintextArray
                -> ExternalChan PlaintextArray
                -> ExternalChan ('Struct "camera_data")
                -> ExternalChan PlaintextArray
                -> ExternalChan PlaintextArray
                -> Component e
serverComponent mkCameraVm uartIn2server can2server cameraVm2server server2uartOut server2can =
  component server $ do
    s2c_pt_from_uart <- inputPortChan uartIn2server
    c2s_from_can     <- inputPortChan can2server
    camera_data_in   <- inputPortChan cameraVm2server

    (c2s_pt_to_uart, s2c_to_can) <- liftTower $ do
      camera_chan      <- channel
      cv_producer      <- controllableVehicleProducerInput c2s_from_can
      let cv_producer' =  cv_producer { cameraTargetInputGetRespProducer
                                      = snd camera_chan }
      c2s_pt_to_uart   <- controllableVehicleProducerOutput cv_producer'
      cv_consumer      <- controllableVehicleConsumerInput s2c_pt_from_uart
      s2c_to_can       <- controllableVehicleConsumerOutput cv_consumer
      case mkCameraVm of
        Nothing -> return ()
        Just _ -> do
          periodicCamera (fst camera_chan) camera_data_in (cameraTargetInputGetReqConsumer cv_consumer)
      return ( c2s_pt_to_uart
             , s2c_to_can
             )

    outputPortChan' c2s_pt_to_uart server2uartOut
    outputPortChan' s2c_to_can server2can

canComponent :: ExternalChan PlaintextArray
             -> ExternalChan PlaintextArray
             -> ExternalChan ('Struct "can_message")
             -> ExternalChan ('Stored IBool)
             -> ExternalChan ('Struct "can_message")
             -> ExternalChan ('Stored IBool)
             -> Component e
canComponent server2can can2server canHw2can_recv canHw2can_status can2canHw_send can2canHw_abort =
  component can $ do
    s2c_to_can   <- inputPortChan  server2can
    canRx        <- inputPortChan  canHw2can_recv
    status       <- inputPortChan  canHw2can_status
    send         <- outputPortChan can2canHw_send
    abort        <- outputPortChan can2canHw_abort
    c2s_from_can <- outputPortChan can2server
    let canTx = AbortableTransmit send abort status
    liftTower $ canDatalink canTx canRx c2s_from_can s2c_to_can

canHwComponent :: ExternalChan ('Struct "can_message")
               -> ExternalChan ('Stored IBool)
               -> ExternalChan ('Stored IBool)
               -> ExternalChan ('Struct "can_message")
               -> Component e
canHwComponent can2canHw_send can2canHw_abort canHw2can_status canHw2can_recv =
  component canHw $ do
    (canRx, AbortableTransmit send abort status) <- liftTower canTower
    inputPortChan'  send can2canHw_send
    inputPortChan'  abort can2canHw_abort
    outputPortChan' status canHw2can_status
    outputPortChan' canRx canHw2can_recv

uartInComponent :: (e -> DatalinkMode)
                -> ExternalChan UartPacket
                -> ExternalChan PlaintextArray
                -> Component e
uartInComponent todl uart_in_ext pt_out_ext =
  component uartIn $ do
    uarti <- inputPortChan uart_in_ext
    input <- outputPortChan pt_out_ext
    liftTower $ do
      input_frames <- channel
      decoderChan  <- channel
      s2c_ct_from_uart <- channel

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

      frameBuffer' (snd input_frames) (Milliseconds 5) (Proxy :: Proxy 4) (fst s2c_ct_from_uart)

      commsecDecodeDatalink todl (snd s2c_ct_from_uart) input

uartOutComponent :: (e -> DatalinkMode)
                 -> ExternalChan HXCyphertext
                 -> ExternalChan ('Stored IBool)
                 -> ExternalChan PlaintextArray
                 -> Component e
uartOutComponent todl uartOut2uartHw uartHw2uartOut server2uartOut =
  component uartOut $ do
    output <- inputPortChan server2uartOut
    status <- inputPortChan uartHw2uartOut
    packet <- outputPortChan uartOut2uartHw
    let uarto = BackpressureTransmit packet (status :: ChanOutput ('Stored IBool))
    liftTower $ do
      c2s_ct_to_uart <- channel
      airDataEncodeTower "frame" (snd c2s_ct_to_uart) uarto
      commsecEncodeDatalink todl output (fst c2s_ct_to_uart)

uartHwComponent :: ExternalChan HXCyphertext
                -> ExternalChan UartPacket
                -> ExternalChan ('Stored IBool)
                -> Component e
uartHwComponent uartOut2uartHw uartHw2uartIn uartHw2uartOut =
  component uartHw $ do
    (BackpressureTransmit output status, input) <- liftTower uartTower
    inputPortChan'  output uartOut2uartHw
    outputPortChan' status uartHw2uartOut
    outputPortChan' input uartHw2uartIn


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
