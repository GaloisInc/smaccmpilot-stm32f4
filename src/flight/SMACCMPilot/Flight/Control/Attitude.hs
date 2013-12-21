{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Attitude
  ( AttitudeControl(..)
  , taskAttitudeControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Flight.Types.Sensors         ()
import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.ArmedMode       as A
import qualified SMACCMPilot.Flight.Types.YawMode         as Y
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import qualified SMACCMPilot.Flight.Types.ControlOutput   ()
import qualified SMACCMPilot.Flight.Types.AttControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.PitchRoll
import           SMACCMPilot.Flight.Control.Attitude.YawRate
import           SMACCMPilot.Flight.Control.Attitude.YawUI
import           SMACCMPilot.Flight.Control.Attitude.HeadingControl

data AttitudeControl =
  AttitudeControl
    { att_init   :: forall eff . Ivory eff ()
    , att_update :: forall eff s1 s2 s3
                  . Ref s1 (Struct "sensors_result")
                 -> Ref s2 (Struct "userinput_result")
                 -> Ref s3 (Struct "control_law")
                 -> IFloat
                 -> Ivory eff ()
    , att_output :: forall eff s . Ref s (Struct "controloutput") -> Ivory eff ()
    }

taskAttitudeControl :: FlightParams ParamReader
                    -> DataSource  (Struct "att_control_dbg")
                    -> Task p AttitudeControl
taskAttitudeControl param_reader s_att_dbg = do
  f <- fresh

  attDbgWriter   <- withDataWriter s_att_dbg "att_control_dbg"

  yui            <- taskYawUI
  yaw_ctl        <- taskYawRateControl param_reader
  hctl           <- taskHeadingControl

  pitch_roll_ctl <- taskPitchRollControl param_reader

  let named n = "att_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        prc_init pitch_roll_ctl

      update_proc :: Def ('[ Ref s1 (Struct "sensors_result")
                           , Ref s2 (Struct "userinput_result")
                           , Ref s3 (Struct "control_law")
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens ui cl dt -> body $ do
        armed    <- deref (cl ~> CL.armed_mode)

        when (armed ==? A.armed) $ do
          yaw_mode <- deref (cl ~> CL.yaw_mode)
          yaw_rate_sp <- cond
            [ yaw_mode ==? Y.rate ==> do
                yui_reset yui
                hctl_reset hctl
                deref (ui ~> UI.yaw)
            , yaw_mode ==? Y.heading ==> do
                yui_update yui sens ui dt
                (heading,rate) <- yui_setpoint yui
                hctl_update   hctl heading rate sens dt
                hctl_setpoint hctl
            ]

          pitch_sp <- deref (ui ~> UI.pitch)
          roll_sp  <- deref (ui ~> UI.roll)

          prc_run   pitch_roll_ctl pitch_sp roll_sp (constRef sens)
          yc_run    yaw_ctl        yaw_rate_sp      (constRef sens)

        unless (armed ==? A.armed) $ do
          yui_reset yui
          hctl_reset hctl
          prc_reset pitch_roll_ctl
          yc_reset  yaw_ctl

        call_ debug_proc

      debug_proc :: Def ('[]:->())
      debug_proc = proc (named "debug") $ body $ do
        dbg <- local izero
        yui_write_debug yui dbg
        hctl_write_debug hctl dbg
        writeData attDbgWriter (constRef dbg)

      output_proc :: Def ('[Ref s (Struct "controloutput")]:->())
      output_proc = proc (named "output") $ \ctl-> body $ do
        prc_state pitch_roll_ctl ctl
        yc_state  yaw_ctl        ctl

  taskModuleDef $ do
    incl init_proc
    incl update_proc
    incl debug_proc
    incl output_proc
  return AttitudeControl
    { att_init   = call_ init_proc
    , att_update = call_ update_proc
    , att_output = call_ output_proc
    }

