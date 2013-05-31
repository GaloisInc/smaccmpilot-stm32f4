{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.FlightMode as FM
import qualified SMACCMPilot.Flight.Types.UserInput as UI
import qualified SMACCMPilot.Flight.Types.Sensors as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Flight.Control.Stabilize

controlTask :: DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> ChannelSink (Struct "sensors_result")
            -> ChannelSource (Struct "controloutput")
            -> Task ()
controlTask s_fm s_inpt s_sens s_ctl = do
  sensRxer   <- withChannelReceiver s_sens "sensors"
  fmReader   <- withDataReader s_fm   "flightmode"
  uiReader   <- withDataReader s_inpt "userinput"
  ctlEmitter <- withChannelEmitter s_ctl  "control"
  taskBody $ \schedule -> do
    fm   <- local (istruct [])
    inpt <- local (istruct [])
    ctl  <- local (istruct [])
    eventLoop schedule $ onChannel sensRxer $ \sens -> do
      readData schedule fmReader   fm
      readData schedule uiReader   inpt

      call_ stabilize_run (constRef fm) (constRef inpt) sens ctl
      -- the trivial throttle controller:
      deref (inpt ~> UI.throttle) >>= store (ctl ~> CO.throttle)

      emit schedule ctlEmitter (constRef ctl)

  taskModuleDef $ \_sch -> do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule

