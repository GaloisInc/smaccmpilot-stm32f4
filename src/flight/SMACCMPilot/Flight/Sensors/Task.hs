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

sensorsTask :: (SingI n)
            => ChannelSource n (Struct "sensors_result")
            -> Task ()
sensorsTask s = do
  sensorsEmitter <- withChannelEmitter s "sensors"
  withStackSize 1024
  s_result <- taskLocal "result"
  taskInit $ do
    store (s_result ~> S.valid) false
    emit_ sensorsEmitter (constRef s_result)
    call_ sensors_begin -- time consuming: boots up and calibrates sensors
  onPeriod 10 $ \_now -> do
    call_ sensors_update
    call_ sensors_getstate s_result
    emit_ sensorsEmitter (constRef s_result)

  taskModuleDef $ do
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
