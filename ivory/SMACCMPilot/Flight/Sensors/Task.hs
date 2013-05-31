{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Sensors.Task
  ( sensorsTask
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.Sensors as S

sensorsTask :: ChannelSource (Struct "sensors_result")
            -> Task ()
sensorsTask s = do
  sensorsEmitter <- withChannelEmitter s "sensors"
  p <- withPeriod 10
  withStackSize 1024
  taskBody $ \sch -> do
    s_result <- local (istruct [ S.valid .= ival false ])
    emit sch sensorsEmitter (constRef s_result)
    call_ sensors_begin -- time consuming: boots up and calibrates sensors
    eventLoop sch $ onTimer p $ \_now -> do
      call_ sensors_update
      call_ sensors_getstate s_result
      emit sch sensorsEmitter (constRef s_result)

  taskModuleDef $ \_sch -> do
    depend S.sensorsTypeModule
    inclHeader "flight-support/sensors_capture.h"
    private $ do
      incl sensors_begin
      incl sensors_update
      incl sensors_getstate

sensors_begin :: Def ('[] :-> ())
sensors_begin = externProc "sensors_begin"

sensors_update :: Def ('[] :-> ())
sensors_update = externProc "sensors_update"

sensors_getstate :: Def ('[Ref s (Struct "sensors_result")] :-> ())
sensors_getstate = externProc "sensors_getstate"
