{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight
  ( flight
  , hil
  ) where

import Control.Applicative ((<$>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable(..))

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Commsec.Commsec (commsecModule)

import SMACCMPilot.Flight.Core
import SMACCMPilot.Flight.Motors.Platforms
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.Sensors.Platforms
import SMACCMPilot.Flight.GCS.Tower
import SMACCMPilot.Flight.Recovery
import SMACCMPilot.Flight.GPS
import SMACCMPilot.Flight.Param

import SMACCMPilot.Param

import SMACCMPilot.Hardware.GPS.Types (gpsTypesModule)

import qualified SMACCMPilot.Flight.Commsec.CommsecOpts as C
import qualified SMACCMPilot.Flight.Types.CommsecStatus as S

import           Ivory.BSP.STM32.Peripheral.UART (uartTower)
import qualified Ivory.BSP.STM32F405.UART as UART
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import           Ivory.BSP.STM32.Signalable
import           Ivory.BSP.STM32.PlatformClock

-- | All parameters in the system.
data SysParams f = SysParams
  { sysFlightParams :: FlightParams f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize the system parameter groups.
sysParams :: Monad m => ParamT f m (SysParams f)
sysParams =
  SysParams <$> group "" flightParams

hil :: ( STM32Signal p, InterruptType p ~ F405.Interrupt
       , PlatformClock p, MotorOutput p, SensorOrientation p)
    => C.Options
    -> Tower p ()
hil opts = do
  -- Communication primitives:
  sensors        <- channel
  position       <- channel
  mavlink_ctlreq <- channel
  rc_override    <- channel
  nav_command    <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  commsec_mon_result <- channel' (Proxy :: Proxy 2) (Just (ival S.secure))

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
                      { sensors_in     = snk sensors
                      , position_in    = snk position
                      , params_in      = sysFlightParams snk_params
                      , rcoverride_in  = snk rc_override
                      , navcommand_in  = snk nav_command
                      , ctl_req_in     = snk mavlink_ctlreq
                      , commsec_mon_in = snk commsec_mon_result
                      }

  -- HIL-enabled GCS on uart1:
  (istream, ostream) <- uartTower UART.uart1 57600 (Proxy :: Proxy 1024)

  -- Commsec reporter, to GCS TX from decrypter
  commsec_info       <- channel

  gcsTowerHil "uart1" opts istream ostream
    GCSRequires
      { gcs_ctl_law_in  = controllaw_state core_out
      , gcs_sens_in     = snk sensors
      , gcs_position_in = snk position
      , gcs_ctl_in      = control_out core_out
      , gcs_motors_in   = motors_out core_out
      , gcs_alt_ctl_in  = alt_ctl_state core_out
      , gcs_att_ctl_in  = att_ctl_state core_out
      , gcs_pos_ctl_in  = pos_ctl_state core_out
      , gcs_commsec_in  = snk commsec_info
      , gcs_comm_mon_in = snk commsec_mon_result
      , gcs_nav_law_in  = navlaw_state  core_out
      }
    GCSProvides
      { gcs_ctl_law_req  = src mavlink_ctlreq
      , gcs_rc_override  = src rc_override
      , gcs_nav_command  = src nav_command
      , gcs_commsec_info = src commsec_info
      , gcs_hil_state    = Nothing
      }
    HILRequires
      { hil_sensors_in = src sensors
      , hil_position_in = src position
      }
    paramList

  towerModule (commsecModule opts)
  -- Missing module that comes in via gpsTower:
  towerModule  gpsTypesModule
  towerDepends gpsTypesModule

flight :: ( STM32Signal p, InterruptType p ~ F405.Interrupt
          , PlatformClock p, MotorOutput p
          , SensorOrientation p)
       => C.Options
       -> Tower p ()
flight opts = do
  -- Communication primitives:
  sensors        <- channel
  mavlink_ctlreq <- channel
  rc_override    <- channel
  nav_command    <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- GPS Input on uart6 (valid for all px4fmu platforms)
  gps_position   <- gpsTower UART.uart6
  -- Sensors managed by AP_HAL
  sensorsTower gps_position (src sensors)

  -- monitor valid commsec, tell core result
  commsec_mon_result <- channel' (Proxy :: Proxy 2) (Just (ival S.secure))

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
    { sensors_in      = snk sensors
    , position_in     = gps_position
    , params_in       = sysFlightParams snk_params
    , rcoverride_in   = snk rc_override
    , navcommand_in   = snk nav_command
    , ctl_req_in      = snk mavlink_ctlreq
    , commsec_mon_in  = snk commsec_mon_result
    }

  -- Motor output dependent on platform
  motorOutput (motors_out core_out)

  -- Commsec reporter, from decrypter to GCS TX and monitor
  commsec_info       <- channel

  let gcsTower' name u = do
        (istream, ostream) <- uartTower u 57600 (Proxy :: Proxy 1024)
        gcsTower name opts istream ostream
          GCSRequires
            { gcs_ctl_law_in  = controllaw_state core_out
            , gcs_sens_in     = snk sensors
            , gcs_position_in = gps_position
            , gcs_ctl_in      = control_out core_out
            , gcs_motors_in   = motors_out core_out
            , gcs_alt_ctl_in  = alt_ctl_state core_out
            , gcs_att_ctl_in  = att_ctl_state core_out
            , gcs_pos_ctl_in  = pos_ctl_state core_out
            , gcs_commsec_in  = snk commsec_info
            , gcs_comm_mon_in = snk commsec_mon_result
            , gcs_nav_law_in  = navlaw_state  core_out
            }
          GCSProvides
            { gcs_ctl_law_req  = src mavlink_ctlreq
            , gcs_rc_override  = src rc_override
            , gcs_nav_command  = src nav_command
            , gcs_commsec_info = src commsec_info
            , gcs_hil_state    = Nothing
            }
          paramList

  gcsTower' "uart1" UART.uart1
  gcsTower' "uart5" UART.uart5

  -- Recovery Tasks
  recoveryTower (snk commsec_info) (src commsec_mon_result)

  towerModule (commsecModule opts)
