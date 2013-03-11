{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Motors.Task
  ( motorsTask
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Servos        as S
import qualified SMACCMPilot.Flight.Types.FlightMode    as M

import SMACCMPilot.Util.Periodic

motorsTask :: Sink (Struct "controloutput")
           -> Sink (Struct "flightmode")
           -> Source (Struct "servos")
           -> String -> Task
motorsTask cs ms ss uniquename =
  withSink   "ctlOut"     cs $ \ctlSink ->
  withSink   "flightMode" ms $ \modeSink ->
  withSource "servos"     ss $ \servoSource ->
  let tDef = proc ("motorTaskDef" ++ uniquename) $ body $ do
        s_ctl   <- local (istruct [])
        s_fm    <- local (istruct [])
        s_servo <- local (istruct [])
        call_ apmotors_output_init
        periodic 10 $ do
          sink   ctlSink             s_ctl
          sink   modeSink            s_fm
          call_  apmotors_output_set (constRef s_ctl) (constRef s_fm)
          call_  apmotors_servo_get  s_servo
          source servoSource         (constRef s_servo)

      mDefs = do
        depend C.controlOutputTypeModule
        depend S.servosTypeModule
        depend M.flightModeTypeModule
        depend OS.taskModule
        inclHeader "apmotors_wrapper"
        incl tDef
        private $ do
          incl apmotors_output_init
          incl apmotors_output_set
          incl apmotors_servo_get

  in task tDef mDefs


apmotors_output_init :: Def ('[] :-> ())
apmotors_output_init = externProc "apmotors_output_init"

apmotors_output_set :: Def ('[ ConstRef s1 (Struct "controloutput")
                             , ConstRef s2 (Struct "flightmode") ] :-> ())
apmotors_output_set = externProc "apmotors_output_set "

apmotors_servo_get :: Def ('[Ref s (Struct "servos")] :-> ())
apmotors_servo_get = externProc "apmotors_servo_get"
