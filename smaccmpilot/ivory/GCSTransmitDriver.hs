{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module GCSTransmitDriver where

import Ivory.Language

import IvoryHelpers

import qualified PositionType as P
import qualified ServoType as Serv
import qualified SensorsType as Sens
import qualified MotorsOutputType as M
import qualified UserInputType as U
import qualified UserInputDecode as U
import qualified SMACCMPilot.Param as Param

import Smaccm.Mavlink.Send (useSendModule)

import qualified Smaccm.Mavlink.Messages.Heartbeat as HB
import qualified Smaccm.Mavlink.Messages.Attitude as ATT
import qualified Smaccm.Mavlink.Messages.VfrHud as HUD
import qualified Smaccm.Mavlink.Messages.ServoOutputRaw as SVO
import qualified Smaccm.Mavlink.Messages.GpsRawInt as GRI
import qualified Smaccm.Mavlink.Messages.GlobalPositionInt as GPI
import qualified Smaccm.Mavlink.Messages.ParamValue as PV

--------------------------------------------------------------------
-- Module def

gcsTransmitDriverModule :: Module
gcsTransmitDriverModule = package "gcs_transmit_driver" $ do
  -- send module has only abstract defs so we depend on it in a weird way
  useSendModule
  -- dependencies for all the smaccmpilot types
  depend P.positionModule
  depend Serv.servoModule
  depend Sens.sensorsTypeModule
  depend M.motorsOutputModule
  depend U.userInputModule
  depend Param.paramModule
  -- dependencies for all the smavlink types and senders
  depend HB.heartbeatModule
  depend ATT.attitudeModule
  depend HUD.vfrHudModule
  depend SVO.servoOutputRawModule
  depend GRI.gpsRawIntModule
  depend GPI.globalPositionIntModule
  depend PV.paramValueModule
  -- module has the following methods
  incl sendHeartbeat
  incl sendAttitude
  incl sendVfrHud
  incl sendServoOutputRaw
  incl sendGpsRawInt
  incl sendGlobalPositionInt
  incl sendParamValue
  incl sendParams

sendHeartbeat :: Def ('[ (Ref s1 (Struct "motorsoutput_result"))
                       , (Ref s2 (Struct "userinput_result"))
                       , (Ref s3 (Struct "smavlink_out_channel"))
                       , (Ref s4 (Struct "smavlink_system"))
                       ] :-> ())
sendHeartbeat = proc "gcs_transmit_send_heartbeat" $ 
  \mot user ch sys -> body $ do
  hb <- local (istruct [])
  armed <- (mot ~>* M.armed)
  mode  <- (user ~>* U.mode)
  store (hb ~> HB.custom_mode) (mode_to_ac2mode mode)
  store (hb ~> HB.mavtype)      mavtype_quadrotor
  -- masquerade as an APM so we can use their custom modes, for now
  store (hb ~> HB.autopilot)    autopilot_ardupilotmega
  ifte armed
    (store (hb ~> HB.base_mode) (mavl_armed + mavl_custom_mode))
    (store (hb ~> HB.base_mode) (mavl_custom_mode))
  -- system status stays 0
  store (hb ~> HB.mavlink_version) 3 -- magic number

  call_ HB.heartbeatSend hb ch sys
  retVoid 
  where
  _autopilot_generic      = 0 -- MAV_AUTOPILOT_GENERIC
  autopilot_ardupilotmega = 3 -- MAV_AUTOPILOT_ARDUPILOTMEGA
  mavtype_quadrotor       = 2 -- MAV_TYPE_QUADROTOR

  mavl_armed        = 128  
  mavl_custom_mode  = 1
  ac2mode_stabilize = 0
  ac2mode_alt_hold  = 2
  ac2mode_loiter    = 5
  mode_to_ac2mode :: Uint8 -> Uint32
  mode_to_ac2mode um = foldr translate ac2mode_stabilize t
    where
    translate (umode, ac2mode) c = (um ==? umode) ? (ac2mode, c)
    t = [(U.mode_STABILIZE, ac2mode_stabilize)
        ,(U.mode_ALT_HOLD,  ac2mode_alt_hold)
        ,(U.mode_LOITER,    ac2mode_loiter)
        ]


sendAttitude :: Def ('[ (Ref s1 (Struct "sensors_result"))
                      , (Ref s2 (Struct "smavlink_out_channel"))
                      , (Ref s3 (Struct "smavlink_system"))
                      ] :-> ())
sendAttitude = proc "gcs_transmit_send_attitude" $ \sensors ch sys -> body $ do
  att <- local (istruct [])
  (sensors ~> Sens.time)    `into` (att ~> ATT.time_boot_ms)
  (sensors ~> Sens.roll)    `into` (att ~> ATT.roll)
  (sensors ~> Sens.pitch)   `into` (att ~> ATT.pitch)
  (sensors ~> Sens.yaw)     `into` (att ~> ATT.yaw)
  (sensors ~> Sens.omega_x) `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_y) `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_z) `into` (att ~> ATT.rollspeed)
  call_ ATT.attitudeSend att ch sys
  retVoid 

sendVfrHud :: Def ('[ (Ref s1 (Struct "position_result"))
                    , (Ref s2 (Struct "motorsoutput_result"))
                    , (Ref s3 (Struct "sensors_result"))
                    , (Ref s4 (Struct "smavlink_out_channel"))
                    , (Ref s5 (Struct "smavlink_system"))
                    ] :-> ())
sendVfrHud = proc "gcs_transmit_send_vfrhud" $ \pos mot sens ch sys -> body $ do
  hud <- local (istruct [])
  -- Calculating speed from vx/vy/vz int16s in m/s*100, into float in m/s
  (calcSpeed pos) `resultInto` (hud ~> HUD.groundspeed)
  (calcSpeed pos) `resultInto` (hud ~> HUD.airspeed)
  -- Calculating alt from int32 in milimeters, into float in meters
  (calcAltitude pos) `resultInto` (hud ~> HUD.alt)
  -- Vertical zpeed from vz
  (calcVertSpeed pos) `resultInto` (hud ~> HUD.climb)
  -- Heading from sensors
  (calcHeading sens) `resultInto` (hud ~> HUD.heading)
  -- Throttle from motor output
  (calcThrottle mot) `resultInto` (hud ~> HUD.throttle)
  call_ HUD.vfrHudSend hud ch sys
  retVoid 
  where
  calcSpeed :: Ref s (Struct "position_result") -> Ivory lex () IFloat
  calcSpeed pos = do
    vx <- (pos ~>* P.vx)
    vy <- (pos ~>* P.vy)
    vz <- (pos ~>* P.vz)
    sumsquares <- assign (safeCast (vx * vx + vy * vy + vz * vz) :: IFloat)
    return $ sqrt sumsquares

  calcAltitude :: Ref s (Struct "position_result") -> Ivory lex () IFloat
  calcAltitude pos = do
    milimeters <- (pos ~>* P.gps_alt)
    mm_float <- assign $ toFloat milimeters
    return (mm_float / 1000)

  calcVertSpeed :: (Ref s (Struct "position_result")) -> Ivory lex () IFloat
  calcVertSpeed pos = do
    meterspersec <- (pos ~>* P.vz)
    return $ (safeCast meterspersec :: IFloat)

  calcHeading :: Ref s (Struct "sensors_result") -> Ivory lex () Sint16
  calcHeading sens = do
    radians <- (sens ~>* Sens.yaw)
    degrees <- assign $ 180 / pi * radians
    deg_int <- assign $ fromFloat 0 degrees
    return  deg_int

  calcThrottle :: Ref s (Struct "motorsoutput_result") -> Ivory l () Uint16
  calcThrottle motors = do
    thrFloat <- (motors ~>* M.throttle)
    return $ fromFloat 0 (thrFloat * 100)

sendServoOutputRaw :: Def ('[ (Ref s1 (Struct "servo_result"))
                            , (Ref s2 (Struct "userinput_result"))
                            , (Ref s3 (Struct "smavlink_out_channel"))
                            , (Ref s4 (Struct "smavlink_system"))
                            ] :-> ())
sendServoOutputRaw = proc "gcs_transmit_send_servo_output" $
  \state user ch sys -> body $ do
  msg <- local (istruct [])
  (state ~> Serv.time)   `into` (msg ~> SVO.time_usec)
  (state ~> Serv.servo1) `into` (msg ~> SVO.servo1_raw)
  (state ~> Serv.servo2) `into` (msg ~> SVO.servo2_raw)
  (state ~> Serv.servo3) `into` (msg ~> SVO.servo3_raw)
  (state ~> Serv.servo4) `into` (msg ~> SVO.servo4_raw)
  pit <- (user  ~>* U.pitch)
  roll <- (user  ~>* U.roll)
  thr <- (user  ~>* U.throttle)
  store (msg ~> SVO.servo6_raw) (fromFloat 9999 ((roll + 1) * 100))
  store (msg ~> SVO.servo7_raw) (fromFloat 9999 ((pit + 1) * 100))
  store (msg ~> SVO.servo8_raw) (fromFloat 9999 ((thr + 1) * 100))

  call_ SVO.servoOutputRawSend msg ch sys


sendGpsRawInt :: Def ('[ (Ref s1 (Struct "position_result"))
                       , (Ref s2 (Struct "smavlink_out_channel"))
                       , (Ref s3 (Struct "smavlink_system"))
                       ] :-> ())
sendGpsRawInt = proc "gcs_transmit_send_gps_raw_int" $
  \pos ch sys -> body $ do
  msg <- local (istruct [])
  (pos ~> P.lat)     `into` (msg ~> GRI.lat)
  (pos ~> P.lon)     `into` (msg ~> GRI.lon)
  (pos ~> P.gps_alt) `into` (msg ~> GRI.alt)
  store (msg ~> GRI.eph) 10
  store (msg ~> GRI.epv) 10
  store (msg ~> GRI.vel) 1 -- XXX can calculate this
  store (msg ~> GRI.cog) 359 -- XXX can calulate this
  store (msg ~> GRI.fix_type) 3 -- 3d fix
  store (msg ~> GRI.satellites_visible) 8
  call_ GRI.gpsRawIntSend msg ch sys
  retVoid 

sendGlobalPositionInt :: Def ('[ (Ref s1 (Struct "position_result"))
                               , (Ref s2 (Struct "sensors_result"))
                               , (Ref s3 (Struct "smavlink_out_channel"))
                               , (Ref s4 (Struct "smavlink_system"))
                               ] :-> ())
sendGlobalPositionInt = proc "gcs_transmit_send_global_position_int" $ 
  \pos sens ch sys -> body $ do
  msg <- local (istruct [])
  yawfloat <- (sens ~>* Sens.yaw)
  yawscaled <- assign $ (10*180/pi)*yawfloat -- radians to 10*degrees
  store (msg ~> GPI.hdg) (fromFloat 9999 yawscaled)
  (pos ~> P.lat)     `into` (msg ~> GPI.lat)
  (pos ~> P.lon)     `into` (msg ~> GPI.lon)
  (pos ~> P.gps_alt) `into` (msg ~> GPI.alt)
  (pos ~> P.vx) `into` (msg ~> GPI.vx)
  (pos ~> P.vy) `into` (msg ~> GPI.vy)
  (pos ~> P.vz) `into` (msg ~> GPI.vz)
  call_ GPI.globalPositionIntSend msg ch sys
  retVoid 

-- Import "strncpy" to fill in the string field with the correct
-- behavior and paper over the char/uint8_t difference.
pv_strncpy :: Def ('[ Ref s1 (CArray (Stored Uint8))
                    , ConstRef s2 (CArray (Stored IChar))
                    , Uint32]
                :-> ())
pv_strncpy = importProc "strncpy" "string"

sendParamValue :: Def ('[ Ref s1 (Struct "param_info")
                        , Ref s2 (Struct "smavlink_out_channel")
                        , Ref s3 (Struct "smavlink_system")]
                       :-> ())
sendParamValue = proc "gcs_transmit_send_param_value" $
  \param ch sys -> body $ do
  msg   <- local (istruct [])
  value <- call Param.param_get_float_value param
  store (msg ~> PV.param_value) value
  count <- Param.param_get_count
  store (msg ~> PV.param_count) (fromIx count)
  index <- deref (param ~> Param.param_index)
  store (msg ~> PV.param_index) (fromIx index)
  call_ pv_strncpy (toCArray (msg ~> PV.param_id))
                   (constRef (toCArray (param ~> Param.param_name))) 16
  store (msg ~> PV.param_type) 0 -- FIXME
  call_ PV.paramValueSend msg ch sys

-- | Send the first parameter marked as requested.
sendParams :: Def ('[ Ref s1 (Struct "smavlink_out_channel")
                    , Ref s2 (Struct "smavlink_system")]
                   :-> ())
sendParams = proc "gcs_transmit_send_params" $ \ch sys -> body $ do
  pinfo <- call Param.param_get_requested
  withRef pinfo
          (\info -> do
             store (info ~> Param.param_requested) 0
             call_ sendParamValue info ch sys)
          retVoid
