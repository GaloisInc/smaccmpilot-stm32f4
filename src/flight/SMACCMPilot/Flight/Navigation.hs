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
import qualified SMACCMPilot.Flight.Types.YawMode           as Y
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CR
import qualified SMACCMPilot.Flight.Types.ControlSetpoint   as SP
import qualified SMACCMPilot.Flight.Types.NavCommand        as NC
import qualified SMACCMPilot.Flight.Types.NavLaw            as NL
import qualified SMACCMPilot.Flight.Types.EnableDisable     as E
import qualified SMACCMPilot.Flight.Types.CommsecStatus     as C

import           SMACCMPilot.Flight.Navigation.Position
import           SMACCMPilot.Flight.Navigation.Velocity
import           SMACCMPilot.Flight.Navigation.Failsafe

data NavInputs =
  NavInputs
    { nav_law_req  :: ChannelSource (Struct "control_law_request")
    , nav_ui       :: ChannelSink   (Struct "userinput_result")
    , nav_ctl_law  :: ChannelSink   (Struct "control_law")
    , nav_position :: ChannelSink   (Struct "position")
    , nav_sens     :: ChannelSink   (Struct "sensors_result")
    , nav_cmd      :: ChannelSink   (Struct "nav_command")
    , nav_commsec_mon :: ChannelSink (Stored C.CommsecStatus)
    }

data NavOutputs =
  NavOutputs
    { nav_setpt   :: ChannelSink (Struct "control_setpoint")
    , nav_pos_dbg :: ChannelSink (Struct "pos_control_dbg")
    , nav_law     :: ChannelSink (Struct "nav_law")
    }

navTower :: FlightParams ParamSink -> NavInputs -> Tower p NavOutputs
navTower params nav_inputs = do
  f <- fresh
  let named n = "nav_" ++ n ++ "_" ++ show f
  nav_setpt_chan <- channel
  nav_law_chan   <- channel
  pos_dbg        <- channel

  task "navigation" $ do
    setpt_emitter   <- withChannelEmitter (src nav_setpt_chan) "control_setpt"
    law_req_emitter <- withChannelEmitter (nav_law_req nav_inputs) "ctl_law_req"
    pos_dbg_emitter <- withChannelEmitter (src pos_dbg) "pos_dbg"
    nav_law_emitter <- withChannelEmitter (src nav_law_chan) "nav_law"

    sens_reader     <- withChannelReader  (nav_sens nav_inputs) "sensors"

    param_reader    <- paramReader params

    pos_control     <- taskPositionControl (flightPosition param_reader)
    vel_control     <- taskVelocityControl (flightPosition param_reader)

    ui              <- taskUpdatable (nav_ui nav_inputs)       "ui"
    ctl_law         <- taskUpdatable (nav_ctl_law nav_inputs)  "law"
    pos             <- taskUpdatable (nav_position nav_inputs) "pos"

    n_law           <- taskLocal "nav_law"

    failsafe        <- taskFailsafe params (nav_commsec_mon nav_inputs)
    taskInit $ do
      pos_init pos_control
      vel_init vel_control
      nlaw_init n_law
      fs_init failsafe

    let check_velocity_control_proc :: Def('[]:->IBool)
        check_velocity_control_proc = proc (named "check_velocity_control") $ body $ do
          got_ui  <- updated_ever ui
          got_law <- updated_ever ctl_law
          got_pos <- updated_within_time pos 500
          got_setpoint <- deref (n_law ~> NL.velocity_control)
          when (got_ui .&& got_law .&& got_pos .&& got_setpoint) $ do
            fix <- deref ((updated_value pos) ~> GPS.fix)
            dop <- deref ((updated_value pos) ~> GPS.dop)
            ret (fix ==? GPS.fix_3d .&& dop <? 4.0)
          ret false

        velocity_control_proc :: Def('[Ref s (Struct "control_setpoint") ]:->())
        velocity_control_proc = proc (named "velocity_control") $ \ctl_sp -> body $ do

          dt    <- assign 0.005 -- XXX calc from _t ?
          sens  <- local izero
          _ <- chanRead sens_reader sens

          -- Really, all pos update is doing is scaling the UI into velocity sp
          pos_update pos_control (constRef sens) (updated_value pos)
                                 (updated_value ui) dt

          (x_vel_sp, y_vel_sp) <- pos_output pos_control
          x_vel_sp' <- deref (n_law ~> NL.vel_x_setpt)
          y_vel_sp' <- deref (n_law ~> NL.vel_y_setpt)

          vel_update vel_control
                     (constRef sens)
                     (updated_value pos)
                     (x_vel_sp + x_vel_sp')
                     (y_vel_sp + y_vel_sp')
                     dt

          (pitch_sp, roll_sp) <- vel_output vel_control

          store (ctl_sp ~> SP.pitch) pitch_sp
          store (ctl_sp ~> SP.roll) roll_sp

        velocity_reset_proc :: Def('[]:->())
        velocity_reset_proc = proc (named "reset") $ body $ do
          vel_reset vel_control
          pos_reset  pos_control

    onPeriod (Milliseconds 5) $ \t -> do
      sens  <- local izero
      _ <- chanRead sens_reader sens
      fs_update failsafe n_law sens
      fs <- fs_active failsafe
      when fs $ do
        emit_ nav_law_emitter (constRef n_law)

      cl_req <- local (CR.initControlLawRequest [ CR.time .= ival t ])
      ctl_sp <- local (istruct [ SP.time .= ival t ])
      vel_ready <- call check_velocity_control_proc
      ifte_ vel_ready
        (do armed_mode <- deref ((updated_value ctl_law) ~> CL.armed_mode)
            stab_src   <- deref ((updated_value ctl_law) ~> CL.stab_source)
            store (cl_req ~> CR.set_stab_src_nav) true
            call_ velocity_control_proc ctl_sp
            unless (stab_src ==? CS.nav .&& armed_mode ==? A.armed)
              (call_ velocity_reset_proc))
        (do store (cl_req ~> CR.set_stab_src_nav) false
            call_ velocity_reset_proc)

      -- HACK:
      -- Clear altitude and heading control if yaw mode is rate (mode switch not
      -- in full auto)
      yaw_mode <- deref ((updated_value ctl_law) ~> CL.yaw_mode)
      when (yaw_mode ==? Y.rate) $ do
        store (n_law ~> NL.heading_control) false
        store (n_law ~> NL.altitude_control) false
        emit_ nav_law_emitter (constRef n_law)

      alt_ready <- deref (n_law ~> NL.altitude_control)
      ifte_ alt_ready
        (do deref (n_law ~> NL.alt_setpt) >>=
              store (ctl_sp ~> SP.altitude)
            deref (n_law ~> NL.alt_rate_setpt) >>=
              store (ctl_sp ~> SP.alt_rate)
            store (cl_req ~> CR.set_autothr_src_nav) true)
        (store (cl_req ~> CR.set_autothr_src_nav) false)


      head_ready <- deref (n_law ~> NL.heading_control)
      ifte_ head_ready
        (do head_sp <- deref (n_law ~> NL.heading_setpt)
            store (ctl_sp ~> SP.heading) head_sp
            store (cl_req ~> CR.set_head_src_nav) true)
        (store (cl_req ~> CR.set_head_src_nav) false)

      emit_ law_req_emitter (constRef cl_req)
      emit_ setpt_emitter (constRef ctl_sp)

      dbg <- local izero
      pos_debug pos_control dbg
      vel_debug vel_control dbg
      emit_ pos_dbg_emitter (constRef dbg)

    nav_cmd_evt <- withChannelEvent (nav_cmd nav_inputs) "nav_cmd"
    handle nav_cmd_evt "nav_cmd" $ \cmd -> do
      fs <- fs_active failsafe
      unless fs $ do
        velocity_control <- deref (cmd ~> NC.velocity_control)
        cond_
          [ velocity_control ==? E.enable ==> do
              store (n_law ~> NL.velocity_control) true
              deref (cmd ~> NC.vel_x_setpt) >>= store (n_law ~> NL.vel_x_setpt)
              deref (cmd ~> NC.vel_y_setpt) >>= store (n_law ~> NL.vel_y_setpt)
          , velocity_control ==? E.disable ==> do
              store (n_law ~> NL.velocity_control) false
          ]
        altitude_control <- deref (cmd ~> NC.altitude_control)
        cond_
          [ altitude_control ==? E.enable ==> do
              store (n_law ~> NL.altitude_control) true
              deref (cmd ~> NC.alt_setpt) >>= store (n_law ~> NL.alt_setpt)
              deref (cmd ~> NC.alt_rate_setpt) >>= store (n_law ~> NL.alt_rate_setpt)
          , altitude_control ==? E.disable ==> do
              store (n_law ~> NL.altitude_control) false
          ]
        heading_control <- deref (cmd ~> NC.heading_control)
        cond_
          [ heading_control ==? E.enable ==> do
              store (n_law ~> NL.heading_control) true
              deref (cmd ~> NC.heading_setpt) >>= store (n_law ~> NL.heading_setpt)
          , heading_control ==? E.disable ==> do
              store (n_law ~> NL.heading_control) false
          ]

        emit_ nav_law_emitter (constRef n_law)


    taskModuleDef $ do
      incl check_velocity_control_proc
      incl velocity_control_proc
      incl velocity_reset_proc

  return NavOutputs
    { nav_setpt = snk nav_setpt_chan
    , nav_pos_dbg = snk pos_dbg
    , nav_law = snk nav_law_chan
    }


data Updated a =
  Updated
    { updated_within_time :: forall eff . ITime -> Ivory eff IBool
    , updated_ever  :: forall eff . Ivory eff IBool
    , updated_value :: ConstRef Global a
    }

-- Prototypin some Tower 2 up in here
taskUpdatable :: (IvoryArea a, IvoryZero a)
            => ChannelSink a -> String -> Task p (Updated a)
taskUpdatable chan name = do
  f <- fresh
  let named n = name ++ "_" ++ n ++ "_" ++ show f
  v        <- taskLocal (named "val")
  got      <- taskLocalInit (named "got") (ival false)
  lasttime <- taskLocal (named "lasttime")

  evt <- withChannelEvent chan (named "chan")
  handle evt (named "evt") $ \i -> do
    t <- getTime
    store got true
    store lasttime t
    refCopy v i

  let within_time_proc :: Def('[ITime]:->IBool)
      within_time_proc = proc (named "within_time") $ \dt -> body $ do
        now <- getTime
        lst <- deref lasttime
        ret (now - lst <=? dt)

  taskModuleDef $ incl within_time_proc
  return Updated
    { updated_within_time = call within_time_proc
    , updated_ever        = deref got
    , updated_value       = constRef v
    }

nlaw_init :: Ref s (Struct "nav_law") -> Ivory eff ()
nlaw_init law = do
  store (law ~> NL.velocity_control  ) false -- IBool
  store (law ~> NL.vel_x_setpt       ) 0     -- IFloat
  store (law ~> NL.vel_y_setpt       ) 0     -- IFloat
  store (law ~> NL.position_control  ) false -- IBool
  store (law ~> NL.lat_setpt         ) 0     -- Sint32
  store (law ~> NL.lon_setpt         ) 0     -- Sint32
  store (law ~> NL.altitude_control  ) false -- IBool
  store (law ~> NL.alt_setpt         ) 0     -- IFloat
  store (law ~> NL.alt_rate_setpt    ) 0     -- IFloat
  store (law ~> NL.heading_control   ) false -- IBool
  store (law ~> NL.heading_setpt     ) 0     -- IFloat
  store (law ~> NL.autoland_active   ) false -- IBool
  store (law ~> NL.autoland_complete ) false -- IBool
  store (law ~> NL.time              ) 0     -- Uint32

