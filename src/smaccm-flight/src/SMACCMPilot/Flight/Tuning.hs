{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Tuning
  ( flightTuningParser
  , FlightTuning
  , flightTuningTower
  ) where

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Tower.Config

import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

import           SMACCMPilot.Flight.Tuning.TypeParsers

import           Prelude ()
import           Prelude.Compat

data FlightTuning =
  FlightTuning
    { ft_altitude_rate     :: Init ('Struct "pid_config")
    , ft_altitude_position :: Init ('Struct "pid_config")
    , ft_nominal_throttle  :: Init ('Stored IFloat)
    , ft_throttle_ui       :: Init ('Struct "throttle_ui")
    , ft_attitude_roll     :: Init ('Struct "stab_config")
    , ft_attitude_pitch    :: Init ('Struct "stab_config")
    , ft_yaw_rate          :: Init ('Struct "pid_config")
    , ft_yaw_position      :: Init ('Struct "pid_config")
    }

flightTuningParser :: ConfigParser FlightTuning
flightTuningParser = do
  ft_altitude_rate     <- subsection "altitude" $ subsection "rate"  $ pidConfigParser
  ft_altitude_position <- subsection "altitude" $ subsection "pos"   $ pidConfigParser
  ft_nominal_throttle  <- subsection "altitude" $ (ival <$> ifloatParser "nominal_throttle")
  ft_throttle_ui       <- subsection "throttle_ui" $ throttleUIParser
  ft_attitude_roll     <- subsection "attitude" $ subsection "roll"  $ stabConfigParser
  ft_attitude_pitch    <- subsection "attitude" $ subsection "pitch" $ stabConfigParser
  ft_yaw_rate          <- subsection "attitude" $ subsection "yaw"   $ subsection "rate" $ pidConfigParser
  ft_yaw_position      <- subsection "attitude" $ subsection "yaw"   $ subsection "pos"  $ pidConfigParser
  return FlightTuning{..}

flightTuningTower :: (e -> FlightTuning)
                  -> ControllableVehicleAttrs Attr
                  -> Tower e ()
flightTuningTower totuning attrs = do
  ft <- fmap totuning getEnv
  monitor "flight_tuning_initializer" $ handler systemInit "flight_tuning_init" $ do
    e_altitude_rate     <- attrEmitter (altitudeRatePid attrs)
    e_altitude_position <- attrEmitter (altitudePositionPid attrs)
    e_nominal_throttle  <- attrEmitter (nominalThrottle attrs)
    e_throttle_ui       <- attrEmitter (throttleUi attrs)
    e_attitude_roll     <- attrEmitter (attitudeRollStab attrs)
    e_attitude_pitch    <- attrEmitter (attitudePitchStab attrs)
    e_yaw_rate          <- attrEmitter (yawRatePid attrs)
    e_yaw_position      <- attrEmitter (yawPositionPid attrs)
    callback $ const $ do
      let go :: (IvoryArea a, GetAlloc eff ~ 'Scope s)
             => (FlightTuning -> Init a)
             -> Emitter a
             -> Ivory eff ()
          go lbl e = do
            v <- local (lbl ft)
            emit e (constRef v)

      go ft_altitude_rate     e_altitude_rate
      go ft_altitude_position e_altitude_position
      go ft_nominal_throttle  e_nominal_throttle
      go ft_throttle_ui       e_throttle_ui
      go ft_attitude_roll     e_attitude_roll
      go ft_attitude_pitch    e_attitude_pitch
      go ft_yaw_rate          e_yaw_rate
      go ft_yaw_position      e_yaw_position
