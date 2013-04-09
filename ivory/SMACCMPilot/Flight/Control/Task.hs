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
            -> EventSink (Struct "sensors_result")
            -> EventSource (Struct "controloutput")
            -> Task ()
controlTask s_fm s_inpt s_sens s_ctl = do
  fmReader   <- withDataReader s_fm   "flightmode"
  uiReader   <- withDataReader s_inpt "userinput"
  sensRxer   <- withEventReceiver s_sens "sensors"
  ctlEmitter <- withEventEmitter s_ctl  "control"
  n <- freshname
  taskBody $ proc ("stabilizeTaskDef" ++ n) $ body $ do
    fm   <- local (istruct [])
    inpt <- local (istruct [])
    sens <- local (istruct [])
    ctl  <- local (istruct [])
    forever $ do
      receive  sensRxer sens
      readData fmReader   fm
      readData uiReader   inpt

      call_ stabilize_run fm inpt sens ctl
      -- the trivial throttle controller:
      deref (inpt ~> UI.throttle) >>= store (ctl ~> CO.throttle)

      emit ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule

