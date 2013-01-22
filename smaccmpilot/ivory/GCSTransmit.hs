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


gcsTransmitModule :: Module
gcsTransmitModule = package "gcs_transmit_driver" $ do
  incl sendHeartbeat
  incl sendAttitude
  incl sendVfrHud
  incl sendServoOutputRaw
  incl sendGps


sendHeartbeat :: Def ('[ (Ref (Struct "motorsoutput_result")) ] :-> ())
sendHeartbeat = proc "gcs_transmit_send_heartbeat" $ \moutput -> do
  retVoid 

sendAttitude :: Def ('[ (Ref (Struct "sensors_result")) ] :-> ())
sendAttitude = proc "gcs_transmit_send_attitude" $ \sensors -> do
  retVoid 

sendVfrHud :: Def ('[ (Ref (Struct "sensors_result")) ] :-> ())
sendVfrHud = proc "gcs_transmit_send_vfrhud" $ \sensors -> do
  retVoid 

sendServoOutputRaw :: Def ('[ (Ref (Struct "servo_result")) ] :-> ())
sendServoOutputRaw = proc "gcs_transmit_send_servo_output" $ \servos -> do
  retVoid 

sendGps :: Def ('[ (Ref (Struct "position_result")) ] :-> ())
sendGps = proc "gcs_transmit_send_gps" $ \position-> do
  retVoid 

