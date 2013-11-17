{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

-- Here are the functions that take the state information and pack it into the
-- Mavlink-specific datatypes, passing them to the generated Mavlink packing
-- functions.  Those functions do the packing into structs, add the headers,
-- etc. and pack it into our sendArr array.

module SMACCMPilot.Flight.GCS.Transmit.MessageDriver where

import qualified MonadLib                                       as M
import qualified MonadLib.Monads                                as M

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Hardware.GPS.Types                 as P

import qualified SMACCMPilot.Flight.Types.Motors                as M
import qualified SMACCMPilot.Flight.Types.Sensors               as Sens
import qualified SMACCMPilot.Flight.Types.ControlOutput         as C
import qualified SMACCMPilot.Flight.Types.UserInput             as U
import qualified SMACCMPilot.Flight.Types.FlightMode            as FM
import qualified SMACCMPilot.Flight.Types.FlightModeData        as FM
import qualified SMACCMPilot.Flight.Types.RadioStat             as RStat

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)

import           SMACCMPilot.Mavlink.Send
import qualified SMACCMPilot.Mavlink.Messages.Heartbeat         as HB
import qualified SMACCMPilot.Mavlink.Messages.Attitude          as ATT
import qualified SMACCMPilot.Mavlink.Messages.VfrHud            as HUD
import qualified SMACCMPilot.Mavlink.Messages.ServoOutputRaw    as SVO
import qualified SMACCMPilot.Mavlink.Messages.GpsRawInt         as GRI
import qualified SMACCMPilot.Mavlink.Messages.GlobalPositionInt as GPI
import qualified SMACCMPilot.Mavlink.Messages.ParamValue        as PV
import qualified SMACCMPilot.Mavlink.Messages.VehicleRadio      as VR
import qualified SMACCMPilot.Mavlink.Messages.AltHoldDebug      as AHD

import           SMACCMPilot.Flight.Control.AltHold

--------------------------------------------------------------------

-- Helper for sending data-rate information to the GCS.
packUint32 :: Ix 16
           -> Ref s (Array 16 (Stored Uint8))
           -> Uint32
           -> Ivory eff ()
packUint32 initIx arr val =
  mapM_ (\(ix,b) -> store (arr ! ix) b) (zip ixes bytes)
  where
  ixes  = [initIx, initIx+1, initIx+2, initIx + 3]
  bytes = fst $ M.runState val $ do
    b0 <- ex
    b1 <- ex
    b2 <- ex
    b3 <- ex
    return [b0,b1,b2,b3]
    where ex = M.sets extractByte

-- Helper type for send functions below.
type Sender a = forall s0 s1 .
  Def ('[ Ref s0  (Struct a)
        , Ref s1 (Stored Uint8)
        , Ref s1 (Struct "mavlinkPacket")
        ] :-> ())

-- Data rate info: time since the last good message and how many messages were
-- dropped.
-- mkSendDataRate :: Sender "data_rate_state"
-- mkSendDataRate =
--   proc "gcs_transmit_send_data16"
--   $ \dr seqNum sendStruct -> body
--   $ do
--   msg <- local (istruct [])
--   d   <- dr ~>* R.dropped
--   l   <- dr ~>* R.lastSucc
--   store (msg ~> D.len) 8 -- 4 bytes for the time, 4 for the number dropped
--   packUint32 0 (msg ~> D.data16) d
--   packUint32 4 (msg ~> D.data16) l

--   call_ D.mkData16Sender (constRef msg) seqNum sendStruct
--   retVoid

mkSendHeartbeat :: Def ('[ Ref s1 (Struct "flightmode")
                         , Ref s2 (Stored IBool)
                         , Ref s3 (Stored Uint8)
                         , Ref s3 (Struct "mavlinkPacket")
                         ] :-> ())
mkSendHeartbeat =
  proc "gcs_transmit_send_heartbeat"
  $ \fm ref_armed seqNum sendStruct -> body $ do
  hb <- local (istruct [])
  armed <- deref ref_armed
  mode  <- (fm ~>* FM.mode)
  store (hb ~> HB.custom_mode) (safeCast $ FM.fromFlightMode mode)
  store (hb ~> HB.mavtype)      mavtype_quadrotor
  store (hb ~> HB.autopilot)    autopilot_smaccmpilot
  ifte_ armed
    (store (hb ~> HB.base_mode) (mavl_armed + mavl_custom_mode))
    (store (hb ~> HB.base_mode) (mavl_custom_mode))
  -- system status stays 0
  store (hb ~> HB.mavlink_version) 3 -- magic number

  call_ HB.mkHeartbeatSender (constRef hb) seqNum sendStruct
  retVoid

  where
  _autopilot_generic, autopilot_smaccmpilot, mavtype_quadrotor :: Uint8
  _autopilot_generic    = 0 -- MAV_AUTOPILOT_GENERIC
  autopilot_smaccmpilot = 13 -- MAV_AUTOPILOT_SMACCMPILOT
  mavtype_quadrotor     = 2 -- MAV_TYPE_QUADROTOR

  mavl_armed        = 128
  mavl_custom_mode  = 1

mkSendAttitude :: Sender "sensors_result"
mkSendAttitude =
  proc "gcs_transmit_send_attitude"
  $ \sensors seqNum sendStruct -> body
  $ do
  att <- local (istruct [])
  (sensors ~> Sens.time)    `into` (att ~> ATT.time_boot_ms)
  (sensors ~> Sens.roll)    `into` (att ~> ATT.roll)
  (sensors ~> Sens.pitch)   `into` (att ~> ATT.pitch)
  (sensors ~> Sens.yaw)     `into` (att ~> ATT.yaw)
  (sensors ~> Sens.omega_x) `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_y) `into` (att ~> ATT.pitchspeed)
  (sensors ~> Sens.omega_z) `into` (att ~> ATT.yawspeed)
  call_ ATT.mkAttitudeSender (constRef att) seqNum sendStruct
  retVoid

mkSendVfrHud :: Def ('[ (Ref s0 (Struct "position"))
                      , (Ref s0 (Struct "controloutput"))
                      , (Ref s0 (Struct "sensors_result"))

                      , Ref s1 (Stored Uint8)
                      , Ref s1 (Struct "mavlinkPacket")
                      ] :-> ())
mkSendVfrHud = proc "gcs_transmit_send_vfrhud"
  $ \pos ctl sens seqNum sendStruct -> body
  $ do
  hud <- local (istruct [])
  -- Calculating speed from vx/vy/vz int16s in m/s*100, into float in m/s
  (calcSpeed pos) `resultInto` (hud ~> HUD.groundspeed)
  (calcSpeed pos) `resultInto` (hud ~> HUD.airspeed)
  -- Calculating alt from int32 in milimeters, into float in meters
--  (positionAltitude pos) `resultInto` (hud ~> HUD.alt)
  (deref (sens ~> Sens.baro_alt)) `resultInto` (hud ~> HUD.alt)
  -- Vertical zpeed from vz
  (calcVertSpeed pos) `resultInto` (hud ~> HUD.climb)
  -- Heading from sensors
  (calcHeading sens) `resultInto` (hud ~> HUD.heading)
  -- Throttle from control output
  (calcThrottle ctl) `resultInto` (hud ~> HUD.throttle)
  call_ HUD.mkVfrHudSender (constRef hud) seqNum sendStruct
  retVoid
  where
  calcSpeed :: Ref s (Struct "position") -> Ivory eff IFloat
  calcSpeed pos = do
    -- vground is in cm/sec
    vground <- (pos ~>* P.vground)
    return $ (safeCast vground) / 100.0

  positionAltitude :: Ref s (Struct "position") -> Ivory eff IFloat
  positionAltitude pos = do
    milimeters <- (pos ~>* P.alt)
    mm_float <- assign $ safeCast milimeters
    return (mm_float / 1000)

  calcVertSpeed :: (Ref s (Struct "position")) -> Ivory eff IFloat
  calcVertSpeed pos = do
    -- vdown is in cm/sec. Output is opposite sign (positive is up)
    meterspersec <- (pos ~>* P.vdown)
    return $ -1.0 * ((safeCast meterspersec :: IFloat) / 100.0)

  calcHeading :: Ref s (Struct "sensors_result") -> Ivory eff Sint16
  calcHeading sens = do
    radians <- (sens ~>* Sens.yaw)
    degrees <- assign $ 180 / pi * radians
    deg_int <- assign $ castDefault degrees
    return  deg_int

  calcThrottle :: Ref s (Struct "controloutput") -> Ivory eff Uint16
  calcThrottle control = do
    thrFloat <- (control ~>* C.throttle)
    return $ castDefault (thrFloat * 100)

mkSendServoOutputRaw :: Def ('[ (Ref s0 (Struct "motors"))
                              , (Ref s0 (Struct "controloutput"))
                              , Ref s' (Stored Uint8)
                              , Ref s' (Struct "mavlinkPacket")
                              ] :-> ())
mkSendServoOutputRaw =
  proc "gcs_transmit_send_servo_output"
  $ \state ctl seqNum sendStruct -> body
  $ do
  msg <- local (istruct [])
  -- ArduCopter Quad X numbering scheme:
  (state ~> M.frontright) `motIntoSvo` (msg ~> SVO.servo1_raw)
  (state ~> M.backleft)   `motIntoSvo` (msg ~> SVO.servo2_raw)
  (state ~> M.frontleft)  `motIntoSvo` (msg ~> SVO.servo3_raw)
  (state ~> M.backright)  `motIntoSvo` (msg ~> SVO.servo4_raw)
  -- Send out control values for debugging:
  (ctl ~> C.yaw)        `ctlIntoSvo` (msg ~> SVO.servo5_raw)
  (ctl ~> C.pitch)      `ctlIntoSvo` (msg ~> SVO.servo6_raw)
  (ctl ~> C.roll)       `ctlIntoSvo` (msg ~> SVO.servo7_raw)
  (ctl ~> C.throttle)   `ctlIntoSvo` (msg ~> SVO.servo8_raw)

  call_ SVO.mkServoOutputRawSender (constRef msg) seqNum sendStruct

  where
  -- Scale [0.0f .. 1.0f] into [1100..1900] pwm value
  motIntoSvo mref sref = do
    m <- deref mref
    store sref (castWith 0 ((m * 800) + 1100))
  -- Scale [-1.0f .. 1.0f] into [0..200] debug value
  ctlIntoSvo cref sref = do
    c <- deref cref
    store sref (castWith 9999 ((c * 100) + 100))

mkSendGpsRawInt :: Sender "position"
mkSendGpsRawInt = proc "gcs_transmit_send_gps_raw_int" $
  \pos seqNum sendStruct -> body $ do
  lat       <- deref (pos ~> P.lat)
  lon       <- deref (pos ~> P.lon)
  alt       <- deref (pos ~> P.alt)
  dop       <- deref (pos ~> P.dop)
  vground   <- deref (pos ~> P.vground) -- cm/s
  heading   <- deref (pos ~> P.heading) -- deg
  fix       <- deref (pos ~> P.fix)
  fix_type  <- assign $ (fix ==? P.fix_3d) ? (3, (fix ==? P.fix_2d) ? (2,0))
  num_sv    <- deref (pos ~> P.num_sv)
  msg       <- local (istruct
    [ GRI.time_usec .= ival 0 -- XXX
    , GRI.lat       .= ival lat
    , GRI.lon       .= ival lon
    , GRI.alt       .= ival alt
    , GRI.eph       .= ival (castWith 0 (100.0*dop))
    , GRI.epv       .= ival 65535
    , GRI.vel       .= ival (castWith 0 vground)
    , GRI.cog       .= ival (castWith 0 (100.0*heading))
    , GRI.fix_type  .= ival fix_type
    , GRI.satellites_visible .= ival num_sv
    ])
  call_ GRI.mkGpsRawIntSender (constRef msg) seqNum sendStruct
  retVoid

mkSendGlobalPositionInt :: Def ('[ (Ref s (Struct "position"))
                                 , (Ref s (Struct "sensors_result"))
                                 , Uint32
                                 , Ref s' (Stored Uint8)
                                 , Ref s' (Struct "mavlinkPacket")
                                 ] :-> ())
mkSendGlobalPositionInt = proc "gcs_transmit_send_global_position_int" $
  \pos sens currenttime seqNum sendStruct -> body $ do
  lat       <- deref (pos ~> P.lat)
  lon       <- deref (pos ~> P.lon)
  alt       <- deref (pos ~> P.alt)
  yaw_rads  <- deref (sens ~> Sens.yaw)
  yaw_cd    <- assign $ (100*180/pi)*yaw_rads
  -- velocity north, cm/s:
  vnorth    <- deref (pos ~> P.vnorth)
  -- velocity east, cm/s:
  veast     <- deref (pos ~> P.veast)
  -- velocity up, cm/s
  vdown     <- deref (pos ~> P.vdown)
  msg <- local (istruct
    [ GPI.time_boot_ms .= ival currenttime
    , GPI.lat          .= ival lat
    , GPI.lon          .= ival lon
    , GPI.alt          .= ival alt
    , GPI.relative_alt .= ival alt -- XXX we don't know ground level.
    , GPI.vx           .= ival (castWith 0 vnorth)
    , GPI.vy           .= ival (castWith 0 veast)
    , GPI.vz           .= ival (-1 * (castWith 0 vdown))
    , GPI.hdg          .= ival (castWith 0 yaw_cd)
    ])
  call_ GPI.mkGlobalPositionIntSender (constRef msg) seqNum sendStruct
  retVoid

mkSendParamValue :: Def ('[ Ref s1 (Struct "param_value_msg")
                          , Ref s2 (Stored Uint8)
                          , Ref s2 (Struct "mavlinkPacket")
                          ] :-> ())
mkSendParamValue = proc "gcs_transmit_send_param_value" $
  \msg seqNum sendStruct -> body $ do
  call_ PV.mkParamValueSender (constRef msg) seqNum sendStruct

mkSendRadio :: Def ('[ Ref s1 (Struct "radio_stat")
                     , Ref s2 (Stored Uint8)
                     , Ref s2 (Struct "mavlinkPacket")
                     ] :-> ())
mkSendRadio = proc "gcs_transmit_send_radio" $
  \stat seqNum sendStruct -> body $ do
  rxerrors <- deref (stat ~> RStat.tx_err)
  fixed    <- deref (stat ~> RStat.ecc_errs)
  rssi     <- deref (stat ~> RStat.loc_rssi)
  remrssi  <- deref (stat ~> RStat.rem_rssi)
  txbuf    <- assign 0 -- XXX
  noise    <- deref (stat ~> RStat.loc_noise)
  remnoise <- deref (stat ~> RStat.rem_noise)
  msg      <- local (istruct
    [ VR.rxerrors .= ival rxerrors
    , VR.fixed    .= ival fixed
    , VR.rssi     .= ival rssi
    , VR.remrssi  .= ival remrssi
    , VR.txbuf    .= ival txbuf
    , VR.noise    .= ival noise
    , VR.remnoise .= ival remnoise
    ])
  call_ VR.mkVehicleRadioSender (constRef msg) seqNum sendStruct

mkSendAltHoldDebug :: Def ('[ ConstRef s1 (Struct "alt_hold_state")
                            , Ref      s2 (Stored Uint8)
                            , Ref      s2 (Struct "mavlinkPacket")
                            ] :-> ())
mkSendAltHoldDebug = proc "gcs_transmit_send_alt_hold_debug" $
  \state seqNum sendStruct -> body $ do
  throttle_cruise <- deref    (state ~> ah_throttle_cruise)
  throttle_avg    <- getMaybe (state ~> ah_throttle_avg)    0.0
  target_alt      <- getMaybe (state ~> ah_target_alt)      0.0
  rate_filter     <- getMaybe (state ~> ah_rate_filter)     0.0
  accel_filter    <- getMaybe (state ~> ah_accel_filter)    0.0
  speed_filter    <- getMaybe (state ~> ah_speed_filter)    0.0
  desired_rate    <- deref    (state ~> ah_desired_rate)
  alt_error       <- deref    (state ~> ah_alt_error)
  target_rate     <- deref    (state ~> ah_target_rate)
  current_rate    <- deref    (state ~> ah_current_rate)
  error_rate      <- deref    (state ~> ah_error_rate)
  target_accel    <- deref    (state ~> ah_target_accel)
  angle_boost     <- deref    (state ~> ah_angle_boost)
  msg             <- local $ istruct
    [ AHD.throttle_cruise .= ival throttle_cruise
    , AHD.throttle_avg    .= ival throttle_avg
    , AHD.target_alt      .= ival target_alt
    , AHD.rate_filter     .= ival rate_filter
    , AHD.accel_filter    .= ival accel_filter
    , AHD.speed_filter    .= ival speed_filter
    , AHD.desired_rate    .= ival desired_rate
    , AHD.alt_error       .= ival alt_error
    , AHD.target_rate     .= ival target_rate
    , AHD.current_rate    .= ival current_rate
    , AHD.error_rate      .= ival error_rate
    , AHD.target_accel    .= ival target_accel
    , AHD.angle_boost     .= ival angle_boost
    ]
  call_ AHD.mkAltHoldDebugSender (constRef msg) seqNum sendStruct

senderModules :: Module
senderModules = package "senderModules" $ do
  mapM_ depend mavlinkMessageModules
--  incl mkSendDataRate
  incl mkSendHeartbeat
  incl mkSendAttitude
  incl mkSendVfrHud
  incl mkSendServoOutputRaw
  incl mkSendGpsRawInt
  incl mkSendGlobalPositionInt
  incl mkSendParamValue
  incl mkSendRadio
  incl mkSendAltHoldDebug

  depend P.gpsTypesModule
  depend M.motorsTypeModule
  depend Sens.sensorsTypeModule
  depend C.controlOutputTypeModule
  depend U.userInputTypeModule
  depend FM.flightModeTypeModule
--  depend R.dataRateTypeModule
  depend RStat.radioStatTypeModule
  depend mavlinkSendModule
  depend altHoldModule
