{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.HIL where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Mavlink.Messages.HilState as H
import qualified SMACCMPilot.Flight.Types.Sensors  as S
import qualified SMACCMPilot.Flight.Types.Position as P

hilTranslator :: (SingI n, SingI m)
              => ChannelSink   n (Struct "hil_state_msg")
              -> ChannelSource m (Struct "sensors_result")
              -> DataSource      (Struct "position_result")
              -> Task p ()
hilTranslator hil sens pos = do
  hilevt       <- withChannelEvent   hil  "hil"
  sens_emitter <- withChannelEmitter sens "sens"
  pos_writer   <- withDataWriter     pos  "pos"
  m            <- withGetTimeMillis
  onEvent hilevt $ \h -> do
    time    <- getTimeMillis m
    roll    <- deref (h ~> H.roll)
    pitch   <- deref (h ~> H.pitch)
    yaw     <- deref (h ~> H.yaw)
    omega_x <- deref (h ~> H.rollspeed)
    omega_y <- deref (h ~> H.pitchspeed)
    omega_z <- deref (h ~> H.yawspeed)
    alt_mm  <- deref (h ~> H.alt)
    xacc    <- deref (h ~> H.xacc)
    yacc    <- deref (h ~> H.yacc)
    zacc    <- deref (h ~> H.zacc)
    s <- local $ istruct
      [ S.roll     .= ival roll
      , S.pitch    .= ival pitch
      , S.yaw      .= ival yaw
      , S.omega_x  .= ival omega_x
      , S.omega_y  .= ival omega_y
      , S.omega_z  .= ival omega_z
      , S.baro_alt .= ival ((safeCast alt_mm) / 1000.0)
      , S.xacc     .= ival (safeCast xacc)
      , S.yacc     .= ival (safeCast yacc)
      , S.zacc     .= ival (safeCast zacc)
      , S.time     .= ival time
      ]
    emit_ sens_emitter (constRef s)
    lat <- deref (h ~> H.lat)
    lon <- deref (h ~> H.lon)
    vx  <- deref (h ~> H.vx)
    vy  <- deref (h ~> H.vy)
    vz  <- deref (h ~> H.vz)
    p <- local $ istruct
      [ P.lat     .= ival lat
      , P.lon     .= ival lon
      , P.gps_alt .= ival alt_mm
      , P.vx      .= ival vx
      , P.vy      .= ival vy
      , P.vz      .= ival vz
      , P.time    .= ival time
      ]
    writeData pos_writer (constRef p)


