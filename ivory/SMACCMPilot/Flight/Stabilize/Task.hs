{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Stabilize.Task where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS

import qualified SMACCMPilot.Flight.Types.UserInput as UI
import qualified SMACCMPilot.Flight.Types.Sensors as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Util.Periodic

stabilizeTask :: Sink (Struct "userinput_result")
          -> Sink (Struct "sensors_result")
          -> Source (Struct "controloutput_result")
          -> String -> Task
stabilizeTask s_inpt s_sensors s_ctl  uniquename =
  withSink   "userinput" s_inpt    $ \userinputSink ->
  withSink   "sensors"   s_sensors $ \sensorsSink ->
  withSource "control"   s_ctl     $ \controlSource ->
  let tDef = proc ("stabilizeTaskDef" ++ uniquename) $ body $ do
        inpt <- local (istruct [])
        sens <- local (istruct [])
        ctl  <- local (istruct [])
        periodic 50 $ do
          sink userinputSink   inpt
          sink sensorsSink     sens
          -- probably should perform some computation here.
          source controlSource (constRef ctl)

      mDefs = do
        depend OS.taskModule
        depend UI.userInputTypeModule
        depend SENS.sensorsTypeModule
        depend CO.controlOutputTypeModule
        incl tDef
  in task tDef mDefs
