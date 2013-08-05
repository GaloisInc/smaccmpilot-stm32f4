{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Motors.Task
  ( motorsTask
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Servos        as S
import qualified SMACCMPilot.Flight.Types.FlightMode    as M

motorsTask :: (SingI n)
           => ChannelSink n (Struct "controloutput")
           -> DataSink (Struct "flightmode")
           -> DataSource (Struct "servos")
           -> Task p ()
motorsTask cs ms ss = do
  ctlRxer   <- withChannelReceiver cs "ctlOut"
  fmReader  <- withDataReader ms "flightMode"
  srvWriter <- withDataWriter ss "servos"
  s_fm <- taskLocal "flightMode"
  s_servo <- taskLocal "servos"
  taskInit $
    call_ apmotors_output_init
  onChannel ctlRxer $ \ctl -> do
    readData fmReader  s_fm
    call_ apmotors_output_set ctl (constRef s_fm)
    call_ apmotors_servo_get  s_servo
    writeData srvWriter       (constRef s_servo)

  taskModuleDef $ do
    depend C.controlOutputTypeModule
    depend S.servosTypeModule
    depend M.flightModeTypeModule
    inclHeader "flight-support/apmotors_wrapper.h"
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
