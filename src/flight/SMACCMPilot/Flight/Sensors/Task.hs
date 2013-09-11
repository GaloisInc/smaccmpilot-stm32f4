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
      res <- local (istruct [])
      call_ sensors_getstate res
      emit_ sensorsEmitter (constRef res)
    return init
  taskInit $ begin sm

  taskModuleDef $ do
    depend S.sensorsTypeModule
    inclHeader "flight-support/sensors_capture.h"
    private $ do
      incl sensors_begin
      incl sensors_update
      incl sensors_getstate

sensors_begin :: Def ('[IBool] :-> ())
sensors_begin = externProc "sensors_begin"

sensors_update :: Def ('[] :-> ())
sensors_update = externProc "sensors_update"

sensors_getstate :: Def ('[Ref s (Struct "sensors_result")] :-> ())
sensors_getstate = externProc "sensors_getstate"
