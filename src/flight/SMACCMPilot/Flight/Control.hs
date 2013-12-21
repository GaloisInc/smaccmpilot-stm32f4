{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ControlLaw    as CL
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO
import qualified SMACCMPilot.Flight.Types.AttControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.Stabilize (attStabilizeModule)
import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.ArmedMode     as A

import           SMACCMPilot.Flight.Control.Altitude
import           SMACCMPilot.Flight.Control.Attitude

controlTask :: (SingI n)
            => DataSink (Struct "control_law")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> ChannelSource n (Struct "controloutput")
            -> DataSource (Struct "alt_control_dbg")
            -> DataSource (Struct "att_control_dbg")
            -> FlightParams ParamSink
            -> Task p ()
controlTask s_law s_inpt s_sens s_ctl s_alt_dbg s_att_dbg params = do
  clReader      <- withDataReader s_law  "control_law"
  uiReader      <- withDataReader s_inpt "userinput"
  sensReader    <- withDataReader s_sens "sensors"
  ctlEmitter    <- withChannelEmitter s_ctl  "control"

  param_reader  <- paramReader params

  alt_control    <- taskAltitudeControl (flightAltitude param_reader) s_alt_dbg
  att_control    <- taskAttitudeControl param_reader s_att_dbg

  taskInit $ do
    alt_init alt_control

  onPeriod 5 $ \_now -> do
      cl   <- local izero
      ui   <- local izero
      sens <- local izero
      readData sensReader  sens
      readData clReader    cl
      readData uiReader    ui

      -- Run altitude and attitude controllers
      alt_update alt_control sens ui cl 0.005 -- XXX calc dt?
      att_update att_control sens ui cl 0.005

      -- Defaults for disarmed:
      ctl <- local $ istruct
        [ CO.throttle .= ival 0
        , CO.roll     .= ival 0
        , CO.pitch    .= ival 0
        , CO.yaw      .= ival 0
        ]

      armed <- deref (cl ~> CL.armed_mode)
      when (armed ==? A.armed) $ do
        alt_output alt_control ctl
        att_output att_control ctl

      emit_ ctlEmitter (constRef ctl)


controlModules :: [Module]
controlModules = [ controlPIDModule, attStabilizeModule ]
