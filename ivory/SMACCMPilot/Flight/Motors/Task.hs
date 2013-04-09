{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Motors.Task
  ( motorsTask
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS.Task as Task

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Servos        as S
import qualified SMACCMPilot.Flight.Types.FlightMode    as M

motorsTask :: EventSink (Struct "controloutput")
           -> DataSink (Struct "flightmode")
           -> DataSource (Struct "servos")
           -> Task ()
motorsTask cs ms ss = do
  n <- freshname
  ctlRxer   <- withEventReceiver cs "ctlOut"
  fmReader  <- withDataReader ms "flightMode"
  srvWriter <- withDataWriter ss "servos"
  taskBody $ proc ("motorTaskDef" ++ n) $ body $ do
    s_ctl   <- local (istruct [])
    s_fm    <- local (istruct [])
    s_servo <- local (istruct [])
    call_ apmotors_output_init
    forever $ do
      receive  ctlRxer   s_ctl
      readData fmReader  s_fm
      call_ apmotors_output_set  (constRef s_ctl) (constRef s_fm)
      call_ apmotors_servo_get   s_servo
      writeData srvWriter        (constRef s_servo)

  taskModuleDef $ do
    depend C.controlOutputTypeModule
    depend S.servosTypeModule
    depend M.flightModeTypeModule
    depend Task.taskModule
    inclHeader "flight-support/apmotors_wrapper"
    private $ do
      incl apmotors_output_init
      incl apmotors_output_set
      incl apmotors_servo_get


apmotors_output_init :: Def ('[] :-> ())
apmotors_output_init = externProc "apmotors_output_init"

apmotors_output_set :: Def ('[ ConstRef s1 (Struct "controloutput")
                             , ConstRef s2 (Struct "flightmode") ] :-> ())
apmotors_output_set = externProc "apmotors_output_set"

apmotors_servo_get :: Def ('[Ref s (Struct "servos")] :-> ())
apmotors_servo_get = externProc "apmotors_servo_get"
