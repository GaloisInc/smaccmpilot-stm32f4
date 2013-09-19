{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}

module SMACCMPilot.Flight.Sensors.Task
  ( sensorsTask
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine
import qualified SMACCMPilot.Flight.Types.Sensors as S

import SMACCMPilot.Flight.Sensors.Platforms

sensorsTask :: forall n p
             . (SensorOrientation p, SingI n)
            => ChannelSource n (Struct "sensors_result")
            -> Task p ()
sensorsTask s = do
  m <- withGetTimeMillis
  sensorsEmitter <- withChannelEmitter s "sensors"
  withStackSize 1024

  sm <- stateMachine "sensors_capture" $ mdo
    init <- stateNamed "init" $ entry $ liftIvory $ do
      res <- local (istruct [ S.valid .= ival false ])
      emit_ sensorsEmitter (constRef res)
      -- time consuming: boots up and calibrates sensors
      call_ sensors_begin (sensorOrientation (Proxy :: Proxy p))
      return $ goto loop

    loop <- stateNamed "captureloop" $ period 10 $ liftIvory_ $ do
      call_ sensors_update
      time <- getTimeMillis m

      (rpy :: Ref (Stack cs) (Array 3 (Stored IFloat))) <- local (iarray [])
      call_ sensors_get_rpy (toCArray rpy)
      roll  <- deref (rpy ! 0)
      pitch <- deref (rpy ! 1)
      yaw   <- deref (rpy ! 2)

      (omega :: Ref (Stack cs) (Array 3 (Stored IFloat))) <- local (iarray [])
      call_ sensors_get_omega (toCArray omega)
      omega_x <- deref (omega ! 0)
      omega_y <- deref (omega ! 1)
      omega_z <- deref (omega ! 2)

      baro_alt <- call sensors_get_baro_alt

      res <- local $ istruct
        [ S.valid     .= ival true
        , S.roll      .= ival roll
        , S.pitch     .= ival pitch
        , S.yaw       .= ival yaw
        , S.omega_x   .= ival omega_x
        , S.omega_y   .= ival omega_y
        , S.omega_z   .= ival omega_z
        , S.baro_alt  .= ival baro_alt
        , S.xacc      .= ival 0
        , S.yacc      .= ival 0
        , S.zacc      .= ival 0
        , S.time      .= ival time
        ]
      emit_ sensorsEmitter (constRef res)
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

sensors_begin :: Def ('[IBool] :-> ())
sensors_begin = externProc "sensors_begin"

sensors_update :: Def ('[] :-> ())
sensors_update = externProc "sensors_update"

sensors_get_rpy :: Def ('[Ref s (CArray (Stored IFloat))] :-> ())
sensors_get_rpy = externProc "sensors_get_rpy"

sensors_get_omega :: Def ('[Ref s (CArray (Stored IFloat))] :-> ())
sensors_get_omega = externProc "sensors_get_omega"

sensors_get_baro_alt :: Def ('[] :-> IFloat)
sensors_get_baro_alt = externProc "sensors_get_baro_alt"
