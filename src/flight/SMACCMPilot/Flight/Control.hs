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

import qualified SMACCMPilot.Flight.Types.AttControlDebug ()
import qualified SMACCMPilot.Flight.Types.AltControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.Stabilize (attStabilizeModule)
import           SMACCMPilot.Param
import           SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.ArmedMode       as A
import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.ControlOutput   as CO
import qualified SMACCMPilot.Flight.Types.ControlSource   as CS
import qualified SMACCMPilot.Flight.Types.ControlSetpoint as SP
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import qualified SMACCMPilot.Flight.Types.YawMode         as Y


import           SMACCMPilot.Flight.Control.Altitude
import           SMACCMPilot.Flight.Control.Attitude.PitchRoll
import           SMACCMPilot.Flight.Control.Yaw
import           SMACCMPilot.Flight.Control.Attitude.YawUI

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
    clReader      <- withDataReader (ci_law   inputs) "control_law"
    uiReader      <- withDataReader (ci_ui    inputs) "userinput"
    sensReader    <- withDataReader (ci_sens  inputs) "sensors"
    navSpReader   <- withDataReader (ci_setpt inputs) "nav_setpt"

    ctlEmitter    <- withChannelEmitter (src ctlout) "control"

    param_reader  <- paramReader params

    alt_control    <- taskAltitudeControl (flightAltitude param_reader)
                                          (src alt_dbg)
    prc_control    <- taskPitchRollControl param_reader
    yaw_control    <- taskYawControl       param_reader
    yui            <- taskYawUI (flightYawUISens param_reader)

    taskInit $ do
      alt_init alt_control
      prc_init prc_control
      yaw_init yaw_control

    onPeriod 5 $ \_now -> do
        dt   <- assign 0.005 -- XXX calc from _now ?
        cl   <- local izero
        ui   <- local izero
        sens <- local izero
        nav_sp <- local izero
        readData sensReader  sens
        readData clReader    cl
        readData uiReader    ui
        readData navSpReader nav_sp

        -- Run altitude and attitude controllers
        alt_update alt_control sens ui nav_sp cl dt

        armed <- deref (cl ~> CL.armed_mode)
        stab_source <- deref (cl ~> CL.stab_source)
        cond_
          [ armed /=? A.armed ==>
              prc_reset prc_control
          , stab_source ==? CS.ui ==> do
              pit_ui <- deref (ui ~> UI.pitch)
              rll_ui <- deref (ui ~> UI.roll)
              ui_sens_dps <- paramGet (flightPRUISens param_reader)
              ui_sens_rads <- assign (ui_sens_dps * pi / 180)
              prc_run prc_control (pit_ui * ui_sens_rads)
                                  (rll_ui * ui_sens_rads)
                                  (constRef sens)
          , stab_source ==? CS.nav ==> do
              pit_sp <- deref (nav_sp ~> SP.pitch)
              rll_sp <- deref (nav_sp ~> SP.roll)
              prc_run prc_control pit_sp rll_sp (constRef sens)
          ]

        yaw_mode <- deref (cl ~> CL.yaw_mode)
        head_source <- deref (cl ~> CL.head_source)
        cond_
          [ armed /=? A.armed ==> do
              yui_reset yui
              yaw_reset yaw_control
          , yaw_mode ==? Y.rate ==> do
              yui_reset yui
              rate_sp <- deref (ui ~> UI.yaw)
              ui_sens_dps <- paramGet (flightYawUISens param_reader)
              ui_sens_rads <- assign (ui_sens_dps * pi / 180)
              yaw_rate yaw_control sens (ui_sens_rads * rate_sp) dt
          , yaw_mode ==? Y.heading .&& head_source ==? CS.ui ==> do
              yui_update yui sens ui dt
              (head_sp, head_rate_sp) <- yui_setpoint yui
              yaw_heading yaw_control sens head_sp head_rate_sp dt
          , yaw_mode ==? Y.heading .&& head_source ==? CS.nav ==> do
              yui_reset yui
              head_sp <- deref (nav_sp ~> SP.heading)
              yaw_heading yaw_control sens head_sp 0 dt
          ]


        -- Defaults for disarmed:
        ctl <- local $ istruct
          [ CO.throttle .= ival 0
          , CO.roll     .= ival 0
          , CO.pitch    .= ival 0
          , CO.yaw      .= ival 0
          ]

        when (armed ==? A.armed) $ do
          alt_output alt_control ctl
          prc_state  prc_control ctl
          yaw_output yaw_control ctl

        emit_ ctlEmitter (constRef ctl)

  mapM_ addModule controlModules

  return ControlOutputs
    { co_ctl     = snk ctlout
    , co_alt_dbg = snk alt_dbg
    , co_att_dbg = snk att_dbg
    }

controlModules :: [Module]
controlModules = [ controlPIDModule, attStabilizeModule ]

