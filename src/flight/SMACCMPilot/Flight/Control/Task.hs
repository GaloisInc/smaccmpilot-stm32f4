{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ArmedMode     as A
import qualified SMACCMPilot.Flight.Types.ThrottleMode  as TM
import qualified SMACCMPilot.Flight.Types.ControlLaw    as CL
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Param

import SMACCMPilot.Flight.Control.Altitude.AutoThrottle
import SMACCMPilot.Flight.Control.Altitude.ThrottleTracker (manual_throttle)

controlTask :: (SingI n)
            => DataSink (Struct "control_law")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> ChannelSource n (Struct "controloutput")
            -> DataSource (Struct "alt_control_dbg")
            -> FlightParams ParamSink
            -> Task p ()
controlTask s_law s_inpt s_sens s_ctl s_ac_state params = do
  clReader      <- withDataReader s_law  "control_law"
  uiReader      <- withDataReader s_inpt "userinput"
  sensReader    <- withDataReader s_sens "sensors"
  acWriter      <- withDataWriter s_ac_state "alt_control_dbg"
  ctlEmitter    <- withChannelEmitter s_ctl  "control"

  param_reader  <- paramReader params
  let stabilize_run = makeStabilizeRun param_reader

  auto_throttle <- taskAutoThrottle (flightAltitude param_reader) acWriter

  taskInit $ do
    at_init auto_throttle

  -- These locals could be on the period handler stack except for some
  -- polymorphism issues with the stabilize_run function...
  cl     <- taskLocal "controllaw"
  ui     <- taskLocal "userinput"
  sens   <- taskLocal "sensors"
  ctl    <- taskLocal "controlout"

  onPeriod 5 $ \_now -> do

      readData sensReader  sens
      readData clReader    cl
      readData uiReader    ui

      thr_mode <- deref (cl ~> CL.thr_mode)
      armed    <- deref (cl ~> CL.armed_mode)

      -- Run stabilizer, which will write to ctl roll, pitch, yaw
      call_ stabilize_run (constRef cl) (constRef ui) (constRef sens) ctl

      -- Run auto throttle controller
      at_update auto_throttle sens ui cl 0.005 -- XXX calc dt?

      -- Set the output throttle accordong to our mode
      store (ctl ~> CO.throttle) =<< cond
        [ armed    /=? A.armed         ==> return 0
        , thr_mode ==? TM.autothrottle ==> at_output auto_throttle
        , true                         ==> manual_throttle ui
        ]
      emit_ ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend stabilizeControlLoopsModule
    incl stabilize_run

