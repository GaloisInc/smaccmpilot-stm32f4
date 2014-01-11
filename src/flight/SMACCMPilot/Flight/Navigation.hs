{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Navigation
  ( navTower
  , NavInputs(..)
  , NavOutputs(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.Param


import qualified SMACCMPilot.Hardware.GPS.Types             as GPS

import           SMACCMPilot.Flight.Types.UserInput ()
import qualified SMACCMPilot.Flight.Types.ControlLaw        as CL
import qualified SMACCMPilot.Flight.Types.ControlSource     as CS
import qualified SMACCMPilot.Flight.Types.ArmedMode         as A
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CR
import qualified SMACCMPilot.Flight.Types.ControlSetpoint   as SP
import qualified SMACCMPilot.Flight.Types.NavCommand        as NC
import qualified SMACCMPilot.Flight.Types.NavLaw            as NL

import           SMACCMPilot.Flight.Navigation.Position
import           SMACCMPilot.Flight.Navigation.Velocity

data NavInputs =
  NavInputs
    { nav_law_req  :: ChannelSource 16 (Struct "control_law_request")
    , nav_ui       :: ChannelSink   16 (Struct "userinput_result")
    , nav_ctl_law  :: ChannelSink   16 (Struct "control_law")
    , nav_position :: ChannelSink   16 (Struct "position")
    , nav_sens     :: DataSink         (Struct "sensors_result")
    , nav_cmd      :: ChannelSink   16 (Struct "nav_command")
    }

data NavOutputs =
  NavOutputs
    { nav_setpt   :: ChannelSink 16 (Struct "control_setpoint")
    , nav_pos_dbg :: DataSink       (Struct "pos_control_dbg")
    , nav_law     :: ChannelSink 16 (Struct "nav_law")
    }

navTower :: FlightParams ParamSink -> NavInputs -> Tower p NavOutputs
navTower params nav_inputs = do
  f <- fresh
  let named n = "nav_" ++ n ++ "_" ++ show f
  nav_setpt_chan <- channel
  nav_law_chan   <- channel
  pos_dbg <- dataport

  task "navigation" $ do
    setpt_emitter   <- withChannelEmitter (src nav_setpt_chan) "control_setpt"
    law_req_emitter <- withChannelEmitter (nav_law_req nav_inputs) "ctl_law_req"
    sens_reader     <- withDataReader     (nav_sens nav_inputs) "sensors"
    pos_dbg_writer  <- withDataWriter     (src pos_dbg) "pos_dbg"
    nav_law_emitter <- withChannelEmitter (src nav_law_chan) "nav_law"

    param_reader    <- paramReader params

    pos_control     <- taskPositionControl (flightPosition param_reader)
    vel_control     <- taskVelocityControl (flightPosition param_reader)

    ui              <- taskUpdatable (nav_ui nav_inputs)       "ui"
    ctl_law         <- taskUpdatable (nav_ctl_law nav_inputs)  "law"
    pos             <- taskUpdatable (nav_position nav_inputs) "pos"

    taskInit $ do
      pos_init pos_control
      vel_init vel_control

    let check_ready_proc :: Def('[]:->IBool)
        check_ready_proc = proc (named "check_ready") $ body $ do
          got_ui  <- updated_ever ui
          got_law <- updated_ever ctl_law
          got_pos <- updated_within_time pos 500
          when (got_ui .&& got_law .&& got_pos) $ do
            fix <- deref ((updated_value pos) ~> GPS.fix)
            dop <- deref ((updated_value pos) ~> GPS.dop)
            ret (fix ==? GPS.fix_3d .&& dop <? 4.0)
          ret false

        run_proc :: Def('[]:->())
        run_proc = proc (named "run") $ body $ do

          dt    <- assign 0.005 -- XXX calc from _t ?
          sens  <- local izero
          readData sens_reader sens

          pos_update pos_control (constRef sens) (updated_value pos)
                                 (updated_value ui) dt

          (x_vel_sp, y_vel_sp) <- pos_output pos_control

          vel_update vel_control (constRef sens) (updated_value pos)
                           x_vel_sp y_vel_sp dt

          (pitch_sp, roll_sp) <- vel_output vel_control

          setpt <- local $ istruct
             [ SP.pitch .= ival pitch_sp
             , SP.roll  .= ival roll_sp
             ]
          emit_ setpt_emitter (constRef setpt)

        reset_proc :: Def('[]:->())
        reset_proc = proc (named "reset") $ body $ do
          vel_reset vel_control
          pos_reset  pos_control

    onPeriod 5 $ \_t -> do
      ready <- call check_ready_proc
      ifte_ ready
        (do armed_mode <- deref ((updated_value ctl_law) ~> CL.armed_mode)
            stab_src   <- deref ((updated_value ctl_law) ~> CL.stab_source)
            ifte_ (stab_src ==? CS.nav .&& armed_mode ==? A.armed)
              (call_ run_proc)
              (do request <- local $ CR.initControlLawRequest
                    [ CR.set_stab_src_nav .= ival true ]
                  emit_ law_req_emitter (constRef request)
                  call_ reset_proc))
        (do request <- local $ CR.initControlLawRequest
              [ CR.set_stab_src_nav .= ival false ]
            emit_ law_req_emitter (constRef request)
            call_ reset_proc)

      dbg <- local izero
      pos_debug pos_control dbg
      vel_debug vel_control dbg
      writeData pos_dbg_writer (constRef dbg)


    onChannel (nav_cmd nav_inputs) "nav_cmd" $ \cmd -> do
      -- XXX USELESS STUB
      law <- local (istruct [])
      emit_ nav_law_emitter (constRef law)


    taskModuleDef $ do
      incl check_ready_proc
      incl run_proc
      incl reset_proc

  return NavOutputs
    { nav_setpt = snk nav_setpt_chan
    , nav_pos_dbg = snk pos_dbg
    , nav_law = snk nav_law_chan
    }


data Updated a =
  Updated
    { updated_within_time :: forall eff . Uint32 -> Ivory eff IBool
    , updated_ever  :: forall eff . Ivory eff IBool
    , updated_value :: ConstRef Global a
    }

-- Prototypin some Tower 2 up in this bitch
taskUpdatable :: (SingI n, IvoryArea a, IvoryZero a)
            => ChannelSink n a -> String -> Task p (Updated a)
taskUpdatable chan name = do
  f <- fresh
  let named n = name ++ "_" ++ n ++ "_" ++ show f
  v        <- taskLocal (named "val")
  got      <- taskLocalInit (named "got") (ival false)
  lasttime <- taskLocal (named "lasttime")
  millis   <- withGetTimeMillis

  onChannel chan (named "chan") $ \i -> do
    t <- getTimeMillis millis
    store got true
    store lasttime t
    refCopy v i

  let within_time_proc :: Def('[Uint32]:->IBool)
      within_time_proc = proc (named "within_time") $ \dt -> body $ do
        now <- getTimeMillis millis
        lst <- deref lasttime
        ret (now - lst <=? dt)

  taskModuleDef $ incl within_time_proc
  return Updated
    { updated_within_time = call within_time_proc
    , updated_ever        = deref got
    , updated_value       = constRef v
    }


