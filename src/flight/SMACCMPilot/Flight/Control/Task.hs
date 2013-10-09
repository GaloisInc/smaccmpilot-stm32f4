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

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Param

controlTask :: (SingI n, SingI m)
            => DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> ChannelSink n (Struct "sensors_result")
            -> ChannelSource m (Struct "controloutput")
            -> FlightParams ParamSink
            -> Task p ()
controlTask s_fm s_inpt s_sens s_ctl params = do
  fmReader   <- withDataReader s_fm   "flightmode"
  uiReader   <- withDataReader s_inpt "userinput"
  ctlEmitter <- withChannelEmitter s_ctl  "control"
  fm   <- taskLocal "flightmode"
  inpt <- taskLocal "input"
  ctl  <- taskLocal "control"

  -- Generate the stabilization function from the parameter set.
  --
  -- TODO: We can generate more than one function here and switch
  -- between them at the call site to use different PID tunings
  -- depending on the flight mode.
  param_reader      <- paramReader params
  let stabilize_run  = makeStabilizeRun param_reader

  onChannel s_sens "sensors" $ \sens -> do
      readData fmReader   fm
      readData uiReader   inpt

      call_ stabilize_run (constRef fm) (constRef inpt) sens ctl
      -- the trivial throttle controller:
      deref (inpt ~> UI.throttle) >>= store (ctl ~> CO.throttle)

      emit_ ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule
    incl stabilize_run
