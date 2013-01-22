{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module GCSTransmit where

import Ivory.Language

import qualified PositionType as P
import qualified ServoType as Serv
import qualified SensorsType as Sens
import qualified MotorsOutputType as M
import qualified UserInputType as U

import qualified Smaccm.Mavlink.Messages.Heartbeat as HB
import qualified Smaccm.Mavlink.Messages.Attitude as ATT
import qualified Smaccm.Mavlink.Messages.VfrHud as HUD
import qualified Smaccm.Mavlink.Messages.ServoOutputRaw as SRVOUT
import qualified Smaccm.Mavlink.Messages.GlobalPositionInt as GPS

-- This is a shorthand for 'deref $ s~>x'.
struct ~>* label = deref $ struct~>label
infixl 8 ~>*

-- Another handy shorthand for transfering members
into :: (IvoryType r, IvoryExpr a) =>
     Ref (Stored a) -> Ref (Stored a) -> Ivory r ()
into a b = do
  v <- deref a
  store b v

--------------------------------------------------------------------
-- Module def

gcsTransmitModule :: Module
gcsTransmitModule = package "gcs_transmit_driver" $ do
  incl sendHeartbeat
  incl sendAttitude
  incl sendVfrHud
  incl sendServoOutputRaw
  incl sendGps

sendHeartbeat :: Def ('[ (Ref (Struct "motorsoutput_result"))
                       , (Ref (Struct "smavlink_out_channel"))
                       , (Ref (Struct "smavlink_system"))
                       ] :-> ())
sendHeartbeat = proc "gcs_transmit_send_heartbeat" $ \mot ch sys -> do
  hb <- local
  call_ HB.heartbeatSend hb ch sys
  retVoid 

sendAttitude :: Def ('[ (Ref (Struct "sensors_result"))
                      , (Ref (Struct "smavlink_out_channel"))
                      , (Ref (Struct "smavlink_system"))
                      ] :-> ())
sendAttitude = proc "gcs_transmit_send_attitude" $ \sensors ch sys -> do
  att <- local
  (sensors ~> Sens.time)    `into` (att ~> ATT.time_boot_ms)
  (sensors ~> Sens.roll)    `into` (att ~> ATT.roll)
  (sensors ~> Sens.pitch)   `into` (att ~> ATT.pitch)
  (sensors ~> Sens.yaw)     `into` (att ~> ATT.yaw)
  (sensors ~> Sens.omega_x) `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_y) `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_z) `into` (att ~> ATT.rollspeed)
  call_ ATT.attitudeSend att ch sys
  retVoid 

sendVfrHud :: Def ('[ (Ref (Struct "sensors_result"))
                    , (Ref (Struct "smavlink_out_channel"))
                    , (Ref (Struct "smavlink_system"))
                    ] :-> ())
sendVfrHud = proc "gcs_transmit_send_vfrhud" $ \s ch sys -> do
  hud <- local
  call_ HUD.vfrHudSend hud ch sys
  retVoid 

sendServoOutputRaw :: Def ('[ (Ref (Struct "servo_result"))
                            , (Ref (Struct "smavlink_out_channel"))
                            , (Ref (Struct "smavlink_system"))
                            ] :-> ())
sendServoOutputRaw = proc "gcs_transmit_send_servo_output" $ \s ch sys -> do
  srv <- local
  call_  SRVOUT.servoOutputRawSend srv ch sys
  retVoid 

sendGps :: Def ('[ (Ref (Struct "position_result"))
                 , (Ref (Struct "smavlink_out_channel"))
                 , (Ref (Struct "smavlink_system"))
                 ] :-> ())
sendGps = proc "gcs_transmit_send_gps" $ \s ch sys -> do
  gps <- local
  call_ GPS.globalPositionIntSend gps ch sys
  retVoid 

