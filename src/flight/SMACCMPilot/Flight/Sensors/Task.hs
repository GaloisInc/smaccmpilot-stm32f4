{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}

module SMACCMPilot.Flight.Sensors.Task
  ( sensorsTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine

import qualified SMACCMPilot.Hardware.GPS.Types as GPS
import qualified SMACCMPilot.Flight.Types.Sensors as S

import SMACCMPilot.Flight.Sensors.Platforms

sensorsTower :: forall n p
             . (SensorOrientation p, SingI n)
            => ChannelSink n (Struct "position")
            -> DataSource  (Struct "sensors_result")
            -> Tower p ()
sensorsTower psnk osrc = task "sensorsCaptureTask" $ do
  sensorsWriter <- withDataWriter osrc "sensors"
  withStackSize 1024

  position <- taskLocal "position"
  onChannel psnk "position" $ \p -> do
    refCopy position p
  let sensorsSetPosition :: Def ('[]:->())
      sensorsSetPosition = proc "sensorsSetPosition" $ body $ do
        fix      <- deref (position ~> GPS.fix)
        num_sv   <- deref (position ~> GPS.num_sv)
        call_ sensors_set_gps_fix (fix ==? GPS.fix_2d) (fix ==? GPS.fix_3d) num_sv
        lat      <- deref (position ~> GPS.lat)
        lon      <- deref (position ~> GPS.lon)
        call_ sensors_set_gps_position lat lon
        vnorth   <- deref (position ~> GPS.vnorth)
        veast    <- deref (position ~> GPS.veast)
        vdown    <- deref (position ~> GPS.vdown)
        vground  <- deref (position ~> GPS.vground)
        heading  <- deref (position ~> GPS.heading)
        call_ sensors_set_gps_velocity vnorth veast vdown vground heading

  taskModuleDef $ incl sensorsSetPosition

  sm <- stateMachine "sensors_capture" $ mdo
    init <- stateNamed "init" $ entry $ liftIvory $ do
      res <- local (istruct [ S.valid .= ival false ])
      writeData sensorsWriter (constRef res)
      -- time consuming: boots up and calibrates sensors
      call_ sensors_begin (sensorOrientation (Proxy :: Proxy p))
      return $ goto loop

    loop <- stateNamed "captureloop" $ period 10 $ liftIvory_ $ do
      call_ sensorsSetPosition
      call_ sensors_update

      (rpy :: Ref (Stack cs) (Array 3 (Stored IFloat))) <- local (iarray [])
      ahrs_time_ref <- local izero
      call_ sensors_get_rpy (toCArray rpy) ahrs_time_ref

      roll      <- deref (rpy ! 0)
      pitch     <- deref (rpy ! 1)
      yaw       <- deref (rpy ! 2)
      ahrs_time <- deref ahrs_time_ref

      (omega :: Ref (Stack cs) (Array 3 (Stored IFloat))) <- local (iarray [])
      call_ sensors_get_omega (toCArray omega)
      omega_x <- deref (omega ! 0)
      omega_y <- deref (omega ! 1)
      omega_z <- deref (omega ! 2)

      (accel :: Ref (Stack cs) (Array 3 (Stored IFloat))) <- local (iarray [])
      call_ sensors_get_accel (toCArray accel)
      accel_x <- deref (accel ! 0)
      accel_y <- deref (accel ! 1)
      accel_z <- deref (accel ! 2)

      baro_time_ref <- local izero
      baro_alt      <- call sensors_get_baro_alt baro_time_ref
      baro_time     <- deref baro_time_ref

      res <- local $ istruct
        [ S.valid     .= ival true
        , S.roll      .= ival roll
        , S.pitch     .= ival pitch
        , S.yaw       .= ival yaw
        , S.omega_x   .= ival omega_x
        , S.omega_y   .= ival omega_y
        , S.omega_z   .= ival omega_z
        , S.baro_alt  .= ival baro_alt
        , S.xacc      .= ival accel_x
        , S.yacc      .= ival accel_y
        , S.zacc      .= ival accel_z
        , S.ahrs_time .= ival ahrs_time
        , S.baro_time .= ival baro_time
        ]
      writeData sensorsWriter (constRef res)
    return init
  taskInit $ begin sm

  taskModuleDef $ do
    depend S.sensorsTypeModule
    inclHeader "apwrapper/sensors_capture.h"
    private $ do
      incl sensors_begin
      incl sensors_update
      incl sensors_get_rpy
      incl sensors_get_omega
      incl sensors_get_baro_alt
      incl sensors_set_gps_velocity
      incl sensors_set_gps_position
      incl sensors_set_gps_fix

sensors_begin :: Def ('[IBool] :-> ())
sensors_begin = externProc "sensors_begin"

sensors_update :: Def ('[] :-> ())
sensors_update = externProc "sensors_update"

sensors_get_rpy :: Def ('[Ref s1 (CArray (Stored IFloat)), Ref s2 (Stored Uint32)] :-> ())
sensors_get_rpy = externProc "sensors_get_rpy"

sensors_get_omega :: Def ('[Ref s (CArray (Stored IFloat))] :-> ())
sensors_get_omega = externProc "sensors_get_omega"

sensors_get_accel :: Def ('[Ref s (CArray (Stored IFloat))] :-> ())
sensors_get_accel = externProc "sensors_get_accel"

sensors_get_baro_alt :: Def ('[Ref s2 (Stored Uint32)] :-> IFloat)
sensors_get_baro_alt = externProc "sensors_get_baro_alt"

-- v_north, v_east, v_down, speed_ground : all in cm/sec
-- heading: in degrees
sensors_set_gps_velocity :: Def('[Sint32, Sint32, Sint32, Uint32, IFloat]:->())
sensors_set_gps_velocity  = externProc "sensors_set_gps_velocity"

-- lat, lon: degrees * 10,000,000
sensors_set_gps_position :: Def('[Sint32, Sint32]:->())
sensors_set_gps_position  = externProc "sensors_set_gps_position"

-- fix2d, fix3d, num_sats
sensors_set_gps_fix      :: Def('[IBool, IBool, Uint8]:->())
sensors_set_gps_fix       = externProc "sensors_set_gps_fix"

