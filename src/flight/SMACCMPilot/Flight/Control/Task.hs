{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.Armed         as A
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.Sensors       as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Control.AltHold
import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Param
import SMACCMPilot.Flight.Types.FlightModeData

import SMACCMPilot.Flight.Control.Altitude.Estimator
import SMACCMPilot.Flight.Control.Altitude.AutoThrottle

controlTask :: (SingI n)
            => DataSink   (Stored A.ArmedMode)
            -> DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> ChannelSource n (Struct "controloutput")
            -> DataSource (Struct "alt_hold_state")
            -> FlightParams ParamSink
            -> Task p ()
controlTask a s_fm s_inpt s_sens s_ctl s_ah_state params = do
  armedReader   <- withDataReader a "armedReader"
  fmReader      <- withDataReader s_fm   "flightmode"
  uiReader      <- withDataReader s_inpt "userinput"
  sensReader    <- withDataReader s_sens "sensors"
  ahWriter      <- withDataWriter s_ah_state "alt_hold_state"
  ctlEmitter    <- withChannelEmitter s_ctl  "control"

  param_reader  <- paramReader params
  let stabilize_run = makeStabilizeRun param_reader

  auto_throttle <- taskAutoThrottle (flightAltHold param_reader) ahWriter

  taskInit $ do
    at_init auto_throttle

  -- These locals could be on the period handler stack except for some
  -- polymorphism issues with the stabilize_run function...
  armRef <- taskLocal "armRef"
  fm     <- taskLocal "flightmode"
  inpt   <- taskLocal "userinput"
  sens   <- taskLocal "sensors"
  ctl    <- taskLocal "controlout"

  onPeriod 5 $ \_now -> do

      readData sensReader  sens
      readData armedReader armRef
      readData fmReader    fm
      readData uiReader    inpt

      mode <- deref (fm ~> FM.mode)
      arm <- deref armRef

      -- Run stabilizer, which will write to ctl roll, pitch, yaw
      call_ stabilize_run arm (constRef fm) (constRef inpt) (constRef sens) ctl

      -- Run auto throttle controller
      at_update auto_throttle sens inpt mode arm 9.99 -- XXX calc dt

      -- Set the output throttle accordong to our mode
      store (ctl ~> CO.throttle) =<< cond
        [ arm  ==? A.as_DISARMED       ==> return 0
        , mode ==? flightModeStabilize ==> manual_throttle inpt
        , true                         ==> at_output auto_throttle
        ]
      emit_ ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule
    depend altHoldModule
    incl stabilize_run

manual_throttle :: Ref s (Struct "userinput_result") -> Ivory eff IFloat
manual_throttle ui = do
  thr <- deref (ui ~> UI.throttle)
  -- -1 =< UI.thr =< 1.  Scale to 0 =< thr' =< 1.
  return ((thr + 1) / 2)

