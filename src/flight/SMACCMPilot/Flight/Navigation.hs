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


import           SMACCMPilot.Flight.Types.UserInput ()
import qualified SMACCMPilot.Flight.Types.ControlLaw        as CL
import qualified SMACCMPilot.Flight.Types.YawMode           as Y
import           SMACCMPilot.Flight.Types.ControlLawRequest ()
import qualified SMACCMPilot.Flight.Types.ControlSetpoint   as SP

import           SMACCMPilot.Flight.Navigation.Position
import           SMACCMPilot.Flight.Navigation.Velocity

data NavInputs =
  NavInputs
    { nav_law_req  :: ChannelSource 16 (Struct "control_law_request")
    , nav_ui       :: ChannelSink   16 (Struct "userinput_result")
    , nav_law      :: ChannelSink   16 (Struct "control_law")
    , nav_position :: ChannelSink   16 (Struct "position")
    , nav_sens     :: DataSink         (Struct "sensors_result")
    }

data NavOutputs =
  NavOutputs
    { nav_setpt   :: ChannelSink 16 (Struct "control_setpoint")
    , nav_pos_dbg :: DataSink       (Struct "pos_control_dbg")
    }

navTower :: FlightParams ParamSink -> NavInputs -> Tower p NavOutputs
navTower params nav_inputs = do
  nav_setpt_chan <- channel
  pos_dbg <- dataport

  task "navigation" $ do
    setpt_emitter   <- withChannelEmitter (src nav_setpt_chan) "control_setpt"
    law_req_emitter <- withChannelEmitter (nav_law_req nav_inputs) "ctl_law_req"
    sens_reader     <- withDataReader     (nav_sens nav_inputs) "sensors"
    millis          <- withGetTimeMillis

    param_reader    <- paramReader params

    pos_control     <- taskPositionControl (flightPosition param_reader)
                                           (src pos_dbg)
    vel_control     <- taskVelocityControl (flightPosition param_reader)

    ui              <- taskUpdatable (nav_ui nav_inputs)       "ui"
    law             <- taskUpdatable (nav_law nav_inputs)      "law"
    pos             <- taskUpdatable (nav_position nav_inputs) "pos"

    taskInit $ do
      pos_init pos_control

    let enabled_proc = proc "nav_enabled" $ body $ do
          ret true

    onPeriod 5 $ \_t -> do
      dt    <- assign 0.005 -- XXX calc from _t ?
      sens  <- local izero
      readData sens_reader sens

      enabled <- call enabled_proc
      ifte_ enabled
        (do pos_update pos_control (constRef sens) (updated_value pos) dt
            (x_vel_sp, y_vel_sp) <- pos_output pos_control
            vel_update vel_control (constRef sens) (updated_value pos)
                             x_vel_sp y_vel_sp dt
            (roll_sp, pitch_sp) <- vel_output vel_control
            setpt <- local $ istruct
               [ SP.roll  .= ival roll_sp
               , SP.pitch .= ival pitch_sp
               ]
            emit_ setpt_emitter (constRef setpt)
        )
        (do vel_reset vel_control
            pos_reset  pos_control
        )

    taskModuleDef $ do
      incl enabled_proc

  return NavOutputs
    { nav_setpt = snk nav_setpt_chan
    , nav_pos_dbg = snk pos_dbg
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


