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
            -> DataSink (Struct "sensors_result")
            -> DataSource (Struct "controloutput")
            -> Task ()
controlTask s_fm s_inpt s_sens s_ctl = do
  fmReader   <- withDataReader s_fm   "flightmode"
  uiReader   <- withDataReader s_inpt "userinput"
  sensReader <- withDataReader s_sens "sensors"
  ctlWriter  <- withDataWriter s_ctl  "control"
  p <- withPeriod 50
  n <- freshname
  taskBody $ proc ("stabilizeTaskDef" ++ n) $ body $ do
    fm   <- local (istruct [])
    inpt <- local (istruct [])
    sens <- local (istruct [])
    ctl  <- local (istruct [])
    periodic p $ do
      readData fmReader   fm
      readData uiReader   inpt
      readData sensReader sens

      call_ stabilize_run fm inpt sens ctl
      -- the trivial throttle controller:
      deref (inpt ~> UI.throttle) >>= store (ctl ~> CO.throttle)

      writeData ctlWriter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule
