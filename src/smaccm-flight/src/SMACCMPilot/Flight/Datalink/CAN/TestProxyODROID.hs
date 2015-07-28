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
    -> Maybe (ChanOutput (Struct "camera_angles"))
    -> Tower e ()
app todl anglesRx = do

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

  case anglesRx of
    Nothing
      -> return ()
    Just cameraAngles
      -> do
        monitor "periodic_camera_injector" $ do
           angles_st   <- stateInit "angles_st"   izero
           angles_prev <- stateInit "angles_prev" izero
           set_req     <- stateInit "set_req"     izero
           seq_num_g   <- stateInit "seq_num_g"   izero

           handler clk "camera_clk" $ do
             e_set <- emitter (fst camera_tgt_req) 1
             callback $ const $ do
               comment ""
               x  <- deref (angles_st ~> angle_x)
               y  <- deref (angles_st ~> angle_y)

               x' <- deref (angles_prev ~> angle_x)
               y' <- deref (angles_prev ~> angle_y)

               unless (x ==? x' .&& y ==? y') $ do
                 refCopy angles_prev angles_st
                 (set_req ~> seqnum) += 1
--                 snum <- deref (set_req ~> seqnum)
                 let ct = set_req ~> val
                 store (ct ~> valid)       true
                 store (ct ~> angle_right) x
                 store (ct ~> angle_up)    y
                 emit e_set (constRef set_req)

           handler cameraAngles "anglesRx" $ do
             callback $ \angles -> do
               refCopy angles_st angles

           handler (cameraTargetInputSetRespProducer cv_producer) "set_response" $ do
             callback $ \seqNum -> do
               comment "camera target setting has been acknowledged"
               s <- deref seqNum
               store seq_num_g s

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
