{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.Armed         as A
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.Sensors       as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Param

controlTask :: (SingI n, SingI m)
            => DataSink   (Stored A.ArmedMode)
            -> DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> ChannelSink n (Struct "sensors_result")
            -> ChannelSource m (Struct "controloutput")
            -> FlightParams ParamSink
            -> Task p ()
controlTask a s_fm s_inpt s_sens s_ctl params = do
  armedReader <- withDataReader a "armedReader"
  fmReader    <- withDataReader s_fm   "flightmode"
  uiReader    <- withDataReader s_inpt "userinput"
  ctlEmitter <- withChannelEmitter s_ctl  "control"
  armRef     <- taskLocal "armed"
  fm         <- taskLocal "flightmode"
  inpt       <- taskLocal "input"
  ctl        <- taskLocal "control"

  -- Generate the stabilization function from the parameter set.
  --
  -- TODO: We can generate more than one function here and switch
  -- between them at the call site to use different PID tunings
  -- depending on the flight mode.
  param_reader      <- paramReader params
  let stabilize_run  = makeStabilizeRun param_reader

  onChannel s_sens "sensors" $ \sens -> do
      readData armedReader armRef
      readData fmReader    fm
      readData uiReader    inpt
      arm <- deref armRef

      call_ stabilize_run arm (constRef fm) (constRef inpt) sens ctl
      -- the trivial throttle controller:
      thr <- deref (inpt ~> UI.throttle)
      -- -1 =< thr =< 1.  Scale to 0 =< thr' =< 1.
      store (ctl ~> CO.throttle) ((thr + 1) / 2)

      emit_ ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule
    incl stabilize_run
