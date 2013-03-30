{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS.Task as Task

import qualified SMACCMPilot.Flight.Types.FlightMode as FM
import qualified SMACCMPilot.Flight.Types.UserInput as UI
import qualified SMACCMPilot.Flight.Types.Sensors as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Flight.Control.Stabilize

import SMACCMPilot.Util.Periodic

controlTask :: DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> DataSource (Struct "controloutput")
            -> String -> Task
controlTask s_fm s_inpt s_sensors s_ctl uniquename =
  withDataSink   "flightmode" s_fm      $ \flightmodeSink->
  withDataSink   "userinput"  s_inpt    $ \userinputSink ->
  withDataSink   "sensors"    s_sensors $ \sensorsSink ->
  withDataSource "control"    s_ctl     $ \controlSource ->
  let tDef = proc ("stabilizeTaskDef" ++ uniquename) $ body $ do
        fm   <- local (istruct [])
        inpt <- local (istruct [])
        sens <- local (istruct [])
        ctl  <- local (istruct [])
        periodic 50 $ do
          dataSink flightmodeSink fm
          dataSink userinputSink  inpt
          dataSink sensorsSink    sens
          call (direct_ stabilize_run fm inpt sens ctl)
          dataSource controlSource (constRef ctl)

      mDefs = do
        depend Task.taskModule
        depend FM.flightModeTypeModule
        depend UI.userInputTypeModule
        depend SENS.sensorsTypeModule
        depend CO.controlOutputTypeModule
        depend stabilizeControlLoopsModule
        incl tDef
  in task tDef mDefs
