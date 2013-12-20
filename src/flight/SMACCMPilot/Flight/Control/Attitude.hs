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
import qualified SMACCMPilot.Flight.Types.UserInput       as UI
import qualified SMACCMPilot.Flight.Types.ControlOutput   ()
import qualified SMACCMPilot.Flight.Types.AttControlDebug ()

import           SMACCMPilot.Flight.Control.Attitude.PitchRoll
import           SMACCMPilot.Flight.Control.Attitude.Yaw

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

  attDbgWriter  <- withDataWriter s_att_dbg "att_control_dbg"

  yaw_ctl        <- taskYawControl       param_reader
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
      update_proc = proc (named "update") $ \sens ui cl _dt -> body $ do
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

        dbg <- local izero
        call_ debug_proc dbg
        writeData attDbgWriter (constRef dbg)

      debug_proc :: Def ('[Ref s (Struct "att_control_dbg")]:->())
      debug_proc = proc (named "debug") $ \out -> body $ do
        return ()

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

