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
import qualified SMACCMPilot.Flight.Types.UserInput             as UI
import qualified SMACCMPilot.Flight.Types.RadioStat             as RStat
import qualified SMACCMPilot.Flight.Types.AltControlDebug       as Alt
import qualified SMACCMPilot.Flight.Types.AttControlDebug       as Att
import qualified SMACCMPilot.Flight.Types.PosControlDebug       as Pos
import qualified SMACCMPilot.Flight.Types.ControlLaw            as CL
import qualified SMACCMPilot.Flight.Types.ThrottleMode          as TM
import qualified SMACCMPilot.Flight.Types.ControlSource         as CS
import qualified SMACCMPilot.Flight.Types.UISource              as US
import qualified SMACCMPilot.Flight.Types.ArmedMode             as A

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
import qualified SMACCMPilot.Mavlink.Messages.AltCtlDebug       as ACD
import qualified SMACCMPilot.Mavlink.Messages.AttCtlDebug       as ACD
import qualified SMACCMPilot.Mavlink.Messages.PosCtlDebug       as PCD
import qualified SMACCMPilot.Mavlink.Messages.VehCommsec        as VC

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

mkSendHeartbeat :: Def ('[ Ref s1 (Struct "control_law")
                         , Ref sm (Stored Uint8)
                         , Ref sm (Struct "mavlinkPacket")
                         ] :-> ())
mkSendHeartbeat =
  proc "gcs_transmit_send_heartbeat"
  $ \cl seqNum sendStruct -> body $ do

  armed_mode  <- deref (cl ~> CL.armed_mode)
  ui_source   <- deref (cl ~> CL.ui_source)
  thr_mode    <- deref (cl ~> CL.thr_mode)
  autothr_src <- deref (cl ~> CL.autothr_source)
  stab_src    <- deref (cl ~> CL.stab_source)
  head_src    <- deref (cl ~> CL.head_source)

  base_mode <- assign $ base_custom_mode
                      + ((armed_mode ==? A.armed) ? (base_armed, 0))
  custom_mode <- assign $
      ((armed_mode ==? A.armed)     ? (custom_armed,    0))
    + ((armed_mode ==? A.disarmed)  ? (custom_disarmed, 0))
    + ((armed_mode ==? A.safe)      ? (custom_safe,     0))
    + ((ui_source  ==? US.ppm)      ? (custom_ui_ppm  * custom_field_ui, 0))
    + ((ui_source  ==? US.mavlink)  ? (custom_ui_mav  * custom_field_ui, 0))
    + ((thr_mode   ==? TM.direct)       ? (custom_thr_direct, 0))
    + ((thr_mode   ==? TM.autothrottle) ? (custom_thr_autothrottle, 0))
    + ((autothr_src ==? CS.ui)     ? (custom_src_ui  * custom_field_athr, 0))
    + ((autothr_src ==? CS.nav)    ? (custom_src_nav * custom_field_athr, 0))
    + ((stab_src    ==? CS.ui)     ? (custom_src_ui  * custom_field_stab, 0))
    + ((stab_src    ==? CS.nav)    ? (custom_src_nav * custom_field_stab, 0))
    + ((head_src    ==? CS.ui)     ? (custom_src_ui  * custom_field_head, 0))
    + ((head_src    ==? CS.nav)    ? (custom_src_nav * custom_field_head, 0))

  hb <- local $ istruct
          [ HB.base_mode       .= ival base_mode
          , HB.custom_mode     .= ival custom_mode
          , HB.mavtype         .= ival mavtype_quadrotor
          , HB.autopilot       .= ival autopilot_smaccmpilot
          , HB.mavlink_version .= ival 3 -- magic number
          ]
  call_ HB.mkHeartbeatSender (constRef hb) seqNum sendStruct
  retVoid

  where
  autopilot_smaccmpilot, mavtype_quadrotor :: Uint8
  autopilot_smaccmpilot = 13 -- MAV_AUTOPILOT_SMACCMPILOT
  mavtype_quadrotor     = 2 -- MAV_TYPE_QUADROTOR

  base_armed        = 128
  base_custom_mode  = 1

  -- ui source is a 1 bit field:
  custom_ui_ppm  = 0
  custom_ui_mav  = 1
  -- control source is a 1 bit field:
  custom_src_ui  = 0
  custom_src_nav  = 1
  -- armed mode is bits 0,1
  custom_safe     = 0
  custom_disarmed = 1
  custom_armed    = 2
  -- ui mode is bit 2
  custom_field_ui = 4
  -- thr mode is bit 3
  custom_thr_direct = 0
  custom_thr_autothrottle = 8
  -- autothrottle src is bit 4
  custom_field_athr = 16
  -- stab src is bit 5
  custom_field_stab = 32
  -- head src is bit 6
  custom_field_head = 64

mkSendAttitude :: Sender "sensors_result"
mkSendAttitude =
  proc "gcs_transmit_send_attitude"
  $ \sensors seqNum sendStruct -> body
  $ do
  att <- local (istruct [])
  (sensors ~> Sens.ahrs_time) `into` (att ~> ATT.time_boot_ms)
  (sensors ~> Sens.roll)      `into` (att ~> ATT.roll)
  (sensors ~> Sens.pitch)     `into` (att ~> ATT.pitch)
  (sensors ~> Sens.yaw)       `into` (att ~> ATT.yaw)
  (sensors ~> Sens.omega_x)   `into` (att ~> ATT.rollspeed)
  (sensors ~> Sens.omega_y)   `into` (att ~> ATT.pitchspeed)
  (sensors ~> Sens.omega_z)   `into` (att ~> ATT.yawspeed)
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

  -- positionAltitude :: Ref s (Struct "position") -> Ivory eff IFloat
  -- positionAltitude pos = do
  --   milimeters <- (pos ~>* P.alt)
  --   mm_float <- assign $ safeCast milimeters
  --   return (mm_float / 1000)

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
    [ GRI.time_usec .= ival 0 -- punting here, FIXME later
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
    , GPI.alt          .= ival alt -- Actually, this is invalid: we're always
    , GPI.relative_alt .= ival alt -- sending relative_alt
    , GPI.vx           .= ival (castWith 0 vnorth)
    , GPI.vy           .= ival (castWith 0 veast)
    , GPI.vz           .= ival 0 -- (-1 * (castWith 0 vdown)) -- XXX FIXME FIXME
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
  txbuf    <- lbits `fmap` deref (stat ~> RStat.rx_ovf)
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

mkSendVehCommsec :: Def ('[ ConstRef s (Struct "veh_commsec_msg")
                          , Ref s2 (Stored Uint8)
                          , Ref s2 (Struct "mavlinkPacket")
                          ] :-> ())
mkSendVehCommsec = proc "gcs_transmit_send_veh_commsec" $
  \commInfo seqNum sendStruct -> body $
    call_ VC.mkVehCommsecSender commInfo seqNum sendStruct

mkSendAltCtlDebug :: Def ('[ ConstRef s1 (Struct "alt_control_dbg")
                           , Ref      s2 (Stored Uint8)
                           , Ref      s2 (Struct "mavlinkPacket")
                           ] :-> ())
mkSendAltCtlDebug = proc "gcs_transmit_send_alt_ctl_debug" $
  \acd seqNum sendStruct -> body $ do
  alt_est        <- deref (acd ~> Alt.alt_est)
  alt_rate_est   <- deref (acd ~> Alt.alt_rate_est)
  thrust_p       <- deref (acd ~> Alt.thrust_p)
  thrust_i       <- deref (acd ~> Alt.thrust_i)
  thrust_d       <- deref (acd ~> Alt.thrust_d)
  thrust_i_reset <- deref (acd ~> Alt.thrust_i_reset)
  ui_setp        <- deref (acd ~> Alt.ui_setp)
  ui_rate_setp   <- deref (acd ~> Alt.ui_rate_setp)
  pos_p          <- deref (acd ~> Alt.pos_p)
  pos_i          <- deref (acd ~> Alt.pos_i)
  pos_d          <- deref (acd ~> Alt.pos_d)
  pos_setp       <- deref (acd ~> Alt.pos_setp)
  pos_rate_setp  <- deref (acd ~> Alt.pos_rate_setp)
  msg            <- local $ istruct
    [ ACD.alt_est         .= ival alt_est
    , ACD.alt_rate_est    .= ival alt_rate_est
    , ACD.thrust_p        .= ival thrust_p
    , ACD.thrust_i        .= ival thrust_i
    , ACD.thrust_d        .= ival thrust_d
    , ACD.thrust_i_reset  .= ival thrust_i_reset
    , ACD.ui_setp         .= ival ui_setp
    , ACD.ui_rate_setp    .= ival ui_rate_setp
    , ACD.pos_p           .= ival pos_p
    , ACD.pos_i           .= ival pos_i
    , ACD.pos_d           .= ival pos_d
    , ACD.pos_setp        .= ival pos_setp
    , ACD.pos_rate_setp   .= ival pos_rate_setp
    ]
  call_ ACD.mkAltCtlDebugSender (constRef msg) seqNum sendStruct

mkSendAttCtlDebug :: Def ('[ ConstRef s1 (Struct "att_control_dbg")
                           , Ref      s2 (Stored Uint8)
                           , Ref      s2 (Struct "mavlinkPacket")
                           ] :-> ())
mkSendAttCtlDebug = proc "gcs_transmit_send_att_ctl_debug" $
  \acd seqNum sendStruct -> body $ do
  head_setpt       <- deref (acd ~> Att.head_setpt)
  head_rate_setpt  <- deref (acd ~> Att.head_rate_setpt)
  head_ctl_p       <- deref (acd ~> Att.head_ctl_p)
  head_ctl_d       <- deref (acd ~> Att.head_ctl_d)
  pitch_setpt      <- deref (acd ~> Att.pitch_setpt)
  pitch_rate_setpt <- deref (acd ~> Att.pitch_rate_setpt)
  roll_setpt       <- deref (acd ~> Att.roll_setpt)
  roll_rate_setpt  <- deref (acd ~> Att.roll_rate_setpt)

  msg             <- local $ istruct
    [ ACD.head_setpt       .= ival head_setpt
    , ACD.head_rate_setpt  .= ival head_rate_setpt
    , ACD.head_ctl_p       .= ival head_ctl_p
    , ACD.head_ctl_d       .= ival head_ctl_d
    , ACD.pitch_setpt      .= ival pitch_setpt
    , ACD.pitch_rate_setpt .= ival pitch_rate_setpt
    , ACD.roll_setpt       .= ival roll_setpt
    , ACD.roll_rate_setpt  .= ival roll_rate_setpt
    ]
  call_ ACD.mkAttCtlDebugSender (constRef msg) seqNum sendStruct

mkSendPosCtlDebug :: Def ('[ ConstRef s1 (Struct "pos_control_dbg")
                           , Ref      s2 (Stored Uint8)
                           , Ref      s2 (Struct "mavlinkPacket")
                           ] :-> ())
mkSendPosCtlDebug = proc "gcs_transmit_send_pos_ctl_debug" $
  \pos seqNum sendStruct -> body $ do
  x_vel_setpt      <- deref (pos ~> Pos.x_vel_setpt)
  y_vel_setpt      <- deref (pos ~> Pos.y_vel_setpt)
  head_setpt       <- deref (pos ~> Pos.head_setpt)
  lat_setpt        <- deref (pos ~> Pos.lat_setpt)
  lon_setpt        <- deref (pos ~> Pos.lon_setpt)
  x_deviation      <- deref (pos ~> Pos.x_deviation)
  y_deviation      <- deref (pos ~> Pos.y_deviation)
  x_vel_est        <- deref (pos ~> Pos.x_vel_est)
  x_vel_p          <- deref (pos ~> Pos.x_vel_p)
  x_vel_i          <- deref (pos ~> Pos.x_vel_i)
  x_vel_d          <- deref (pos ~> Pos.x_vel_d)
  y_vel_est        <- deref (pos ~> Pos.y_vel_est)
  y_vel_p          <- deref (pos ~> Pos.y_vel_p)
  y_vel_i          <- deref (pos ~> Pos.y_vel_i)
  y_vel_d          <- deref (pos ~> Pos.y_vel_d)

  msg             <- local $ istruct
    [ PCD.x_vel_setpt      .= ival x_vel_setpt
    , PCD.y_vel_setpt      .= ival y_vel_setpt
    , PCD.head_setpt       .= ival head_setpt
    , PCD.lat_setpt        .= ival lat_setpt
    , PCD.lon_setpt        .= ival lon_setpt
    , PCD.x_deviation      .= ival x_deviation
    , PCD.y_deviation      .= ival y_deviation
    , PCD.x_vel_est        .= ival x_vel_est
    , PCD.x_vel_p          .= ival x_vel_p
    , PCD.x_vel_i          .= ival x_vel_i
    , PCD.x_vel_d          .= ival x_vel_d
    , PCD.y_vel_est        .= ival y_vel_est
    , PCD.y_vel_p          .= ival y_vel_p
    , PCD.y_vel_i          .= ival y_vel_i
    , PCD.y_vel_d          .= ival y_vel_d
    ]
  call_ PCD.mkPosCtlDebugSender (constRef msg) seqNum sendStruct

senderModules :: Module
senderModules = package "senderModules" $ do
  mapM_ depend mavlinkMessageModules
  incl mkSendHeartbeat
  incl mkSendAttitude
  incl mkSendVfrHud
  incl mkSendServoOutputRaw
  incl mkSendGpsRawInt
  incl mkSendGlobalPositionInt
  incl mkSendParamValue
  incl mkSendRadio
  incl mkSendVehCommsec
  incl mkSendAltCtlDebug
  incl mkSendAttCtlDebug
  incl mkSendPosCtlDebug

  depend P.gpsTypesModule
  depend M.motorsTypeModule
  depend Sens.sensorsTypeModule
  depend C.controlOutputTypeModule
  depend UI.userInputTypeModule
  depend CL.controlLawTypeModule
  depend RStat.radioStatTypeModule
  depend VR.vehicleRadioModule
  depend VC.vehCommsecModule
  depend Alt.altControlDebugTypeModule
  depend Att.attControlDebugTypeModule
  depend Pos.posControlDebugTypeModule
  depend mavlinkSendModule
