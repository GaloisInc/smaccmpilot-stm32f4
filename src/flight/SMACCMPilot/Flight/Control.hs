{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ThrottleMode  as TM
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.ControlLaw    as CL
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.ArmedMode     as A
import           SMACCMPilot.Flight.Control.Stabilize.Control (stabCtlModule)
import           SMACCMPilot.Flight.Control.Stabilize.PitchRoll
import           SMACCMPilot.Flight.Control.Stabilize.Yaw

import           SMACCMPilot.Flight.Control.Altitude.AutoThrottle
import           SMACCMPilot.Flight.Control.Altitude.ThrottleTracker (manual_throttle)

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

  pitch_roll_ctl <- taskPitchRollControl param_reader
  yaw_ctl        <- taskYawControl       param_reader
  auto_throttle <- taskAutoThrottle (flightAltitude param_reader) acWriter

  taskInit $ do
    at_init auto_throttle
    prc_init pitch_roll_ctl


  onPeriod 5 $ \_now -> do
      cl   <- local izero
      ui   <- local izero
      sens <- local izero
      readData sensReader  sens
      readData clReader    cl
      readData uiReader    ui

      armed    <- deref (cl ~> CL.armed_mode)

      -- Run yaw pitch roll controller
      when (armed ==? A.armed) $ do
        pitch_sp <- deref (ui ~> UI.pitch)
        roll_sp  <- deref (ui ~> UI.roll)
        yaw_sp   <- deref (ui ~> UI.yaw)

        prc_run   pitch_roll_ctl pitch_sp roll_sp (constRef sens)
        yc_run    yaw_ctl        yaw_sp           (constRef sens)

      unless (armed ==? A.armed) $ do
        prc_reset pitch_roll_ctl
        yc_reset  yaw_ctl

      -- Run auto throttle controller
      at_update auto_throttle sens ui cl 0.005 -- XXX calc dt?

      -- Set the output throttle according to throttle law
      thr_mode <- deref (cl ~> CL.thr_mode)
      thr <- cond
        [ armed    /=? A.armed         ==> return 0
        , thr_mode ==? TM.autothrottle ==> at_output auto_throttle
        , true                         ==> manual_throttle ui
        ]

      -- Gather outputs and emit:
      ctl  <- local $ istruct [ CO.throttle .= ival thr ]
      prc_state pitch_roll_ctl ctl
      yc_state  yaw_ctl        ctl
      emit_ ctlEmitter (constRef ctl)


controlModules :: [Module]
controlModules = [ controlPIDModule, stabCtlModule ]
