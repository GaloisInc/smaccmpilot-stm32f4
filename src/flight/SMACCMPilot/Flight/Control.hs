{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control
  ( controlTower
  , ControlInputs(..)
  , ControlOutputs(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ControlLaw    as CL
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO
import qualified SMACCMPilot.Flight.Types.AttControlDebug ()
import qualified SMACCMPilot.Flight.Types.AltControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.Stabilize (attStabilizeModule)
import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.ArmedMode     as A

import           SMACCMPilot.Flight.Control.Altitude
import           SMACCMPilot.Flight.Control.Attitude

data ControlInputs =
  ControlInputs
    { ci_law   :: DataSink (Struct "control_law")
    , ci_ui    :: DataSink (Struct "userinput_result")
    , ci_setpt :: DataSink (Struct "control_setpoint")
    , ci_sens  :: DataSink (Struct "sensors_result")
    }

data ControlOutputs =
  ControlOutputs
    { co_ctl     :: ChannelSink 16 (Struct "controloutput")
    , co_alt_dbg :: DataSink (Struct "alt_control_dbg")
    , co_att_dbg :: DataSink (Struct "att_control_dbg")
    }

controlTower :: FlightParams ParamSink
            -> ControlInputs
            -> Tower p ControlOutputs
controlTower params inputs = do
  ctlout <- channel
  alt_dbg <- dataport
  att_dbg <- dataport
  task "control" $ do
    clReader      <- withDataReader (ci_law  inputs) "control_law"
    uiReader      <- withDataReader (ci_ui   inputs) "userinput"
    sensReader    <- withDataReader (ci_sens inputs) "sensors"

    ctlEmitter    <- withChannelEmitter (src ctlout) "control"

    param_reader  <- paramReader params

    alt_control    <- taskAltitudeControl (flightAltitude param_reader)
                                          (src alt_dbg)
    att_control    <- taskAttitudeControl param_reader
                                          (src att_dbg)
    taskModuleDef $ do
      depend controlPIDModule -- for fconstrain
    taskInit $ do
      alt_init alt_control
      att_init att_control

    onPeriod 5 $ \_now -> do
        dt   <- assign 0.005 -- XXX calc from _now ?
        cl   <- local izero
        ui   <- local izero
        sens <- local izero
        readData sensReader sens
        readData clReader   cl
        readData uiReader   ui

        -- XXX choose setpoint here

        -- Run altitude and attitude controllers
        alt_update alt_control sens ui cl dt
        att_update att_control sens ui cl dt

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

  mapM_ addModule controlModules

  return ControlOutputs
    { co_ctl     = snk ctlout
    , co_alt_dbg = snk alt_dbg
    , co_att_dbg = snk att_dbg
    }

controlModules :: [Module]
controlModules = [ controlPIDModule, attStabilizeModule ]

