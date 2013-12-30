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
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.AttControlDebug ()
import qualified SMACCMPilot.Flight.Types.AltControlDebug ()
import qualified SMACCMPilot.Flight.Types.PosControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.Stabilize (attStabilizeModule)
import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID
import qualified SMACCMPilot.Flight.Types.ArmedMode     as A

import           SMACCMPilot.Flight.Control.Altitude
import           SMACCMPilot.Flight.Control.Attitude
import           SMACCMPilot.Flight.Control.Position

controlTask :: (SingI n)
            => DataSink (Struct "control_law")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> DataSink (Struct "position")
            -> ChannelSource n (Struct "controloutput")
            -> DataSource (Struct "pos_control_dbg")
            -> DataSource (Struct "alt_control_dbg")
            -> DataSource (Struct "att_control_dbg")
            -> FlightParams ParamSink
            -> Task p ()
controlTask s_law s_inpt s_sens s_pos s_ctl s_pos_dbg s_alt_dbg s_att_dbg params = do
  clReader      <- withDataReader s_law  "control_law"
  uiReader      <- withDataReader s_inpt "userinput"
  sensReader    <- withDataReader s_sens "sensors"
  posReader     <- withDataReader s_pos  "position"
  ctlEmitter    <- withChannelEmitter s_ctl  "control"

  param_reader  <- paramReader params

  pos_control    <- taskPositionControl (flightPosition param_reader) s_pos_dbg
  alt_control    <- taskAltitudeControl (flightAltitude param_reader) s_alt_dbg
  att_control    <- taskAttitudeControl param_reader s_att_dbg

  taskInit $ do
    pos_init pos_control
    alt_init alt_control
    att_init att_control

  onPeriod 5 $ \_now -> do
      dt   <- assign 0.005 -- XXX calc from _now ?
      cl   <- local izero
      ui   <- local izero
      pos  <- local izero
      sens <- local izero
      readData sensReader  sens
      readData clReader    cl
      readData posReader   pos

      ifte_ false -- XXX LAW
        (do pos_update pos_control sens pos dt
            (valid, x, y) <- pos_output pos_control
            when valid $ do
              store (ui ~> UI.pitch) x
              store (ui ~> UI.roll)  y
        )
        (pos_reset  pos_control)

      -- Run altitude and attitude controllers
      readData uiReader ui
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


controlModules :: [Module]
controlModules = [ controlPIDModule, attStabilizeModule ]
