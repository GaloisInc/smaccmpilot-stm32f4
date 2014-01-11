{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

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

import qualified Ivory.BSP.STM32F4.UART as UART
import           Ivory.BSP.STM32F4.RCC (BoardHSE(..))

-- | All parameters in the system.
data SysParams f = SysParams
  { sysFlightParams :: FlightParams f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize the system parameter groups.
sysParams :: Monad m => ParamT f m (SysParams f)
sysParams =
  SysParams <$> group "" flightParams

hil :: (BoardHSE p, MotorOutput p, SensorOrientation p)
    => C.Options
    -> Tower p ()
hil opts = do
  -- Communication primitives:
  sensors        <- dataport
  position       <- channel
  mavlink_ctlreq <- channel
  rc_override    <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
                      { sensors_in    = snk sensors
                      , position_in   = snk position
                      , params_in     = sysFlightParams snk_params
                      , rcoverride_in = snk rc_override
                      , ctl_req_in    = snk mavlink_ctlreq
                      }

  control_state     <- stateProxy "control_state" (control_out core_out)
  motors_state      <- stateProxy "motors_state" (motors_out core_out)
  position_state    <- stateProxy "position_state" (snk position)

  -- HIL-enabled GCS on uart1:
  (istream, ostream) <- uart UART.uart1

  -- Commsec reporter, to GCS TX from decrypter
  commsec_info       <- dataport
  commsec_mon_result <- dataport

  gcsTowerHil "uart1" opts istream ostream
    GCSRequires
      { gcs_ctl_law_in  = controllaw_state core_out
      , gcs_sens_in     = snk sensors
      , gcs_position_in = position_state
      , gcs_ctl_in      = control_state
      , gcs_motors_in   = motors_state
      , gcs_alt_ctl_in  = alt_ctl_state core_out
      , gcs_att_ctl_in  = att_ctl_state core_out
      , gcs_pos_ctl_in  = pos_ctl_state core_out
      , gcs_commsec_in  = snk commsec_info
      , gcs_comm_mon_in = snk commsec_mon_result
      }
    GCSProvides
      { gcs_ctl_law_req  = src mavlink_ctlreq
      , gcs_rc_override  = src rc_override
      , gcs_commsec_info = src commsec_info
      , gcs_hil_state    = Nothing
      }
    HILRequires
      { hil_sensors_in = src sensors
      , hil_position_in = src position
      }
    paramList

  addModule (commsecModule opts)
  -- Missing module that comes in via gpsTower:
  addModule  gpsTypesModule
  addDepends gpsTypesModule

flight :: (BoardHSE p, MotorOutput p, SensorOrientation p)
       => C.Options
       -> Tower p ()
flight opts = do
  -- Communication primitives:
  sensors        <- dataport
  mavlink_ctlreq <- channel
  rc_override    <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- GPS Input on uart6 (valid for all px4fmu platforms)
  gps_position   <- gpsTower UART.uart6
  -- Sensors managed by AP_HAL
  sensorsTower gps_position (src sensors)

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
    { sensors_in    = snk sensors
    , position_in   = gps_position
    , params_in     = sysFlightParams snk_params
    , rcoverride_in = snk rc_override
    , ctl_req_in    = snk mavlink_ctlreq
    }

  control_state     <- stateProxy "control_state" (control_out core_out)
  motors_state      <- stateProxy "motors_state" (motors_out core_out)

  -- Motor output dependent on platform
  motorOutput (motors_out core_out)

  -- Commsec reporter, to GCS TX from decrypter
  commsec_info       <- dataport
  commsec_mon_result <- dataport

  position_state <- stateProxy "position_state" gps_position
  let gcsTower' name istream ostream =
        gcsTower name opts istream ostream
          GCSRequires
            { gcs_ctl_law_in  = controllaw_state core_out
            , gcs_sens_in     = snk sensors
            , gcs_position_in = position_state
            , gcs_ctl_in      = control_state
            , gcs_motors_in   = motors_state
            , gcs_alt_ctl_in  = alt_ctl_state core_out
            , gcs_att_ctl_in  = att_ctl_state core_out
            , gcs_pos_ctl_in  = pos_ctl_state core_out
            , gcs_commsec_in  = snk commsec_info
            , gcs_comm_mon_in = snk commsec_mon_result
            }
          GCSProvides
            { gcs_ctl_law_req  = src mavlink_ctlreq
            , gcs_rc_override  = src rc_override
            , gcs_commsec_info = src commsec_info
            , gcs_hil_state    = Nothing
            }
          paramList

  -- GCS on UART1:
  (uart1istream, uart1ostream) <- uart UART.uart1
  gcsTower' "uart1" uart1istream uart1ostream

  -- GCS on UART5:
  (uart5istream, uart5ostream) <- uart UART.uart5
  gcsTower' "uart5" uart5istream uart5ostream

  -- Recovery Tasks
  recoveryTower (snk commsec_info) (src commsec_mon_result)

  addModule (commsecModule opts)

-- Helper: a uartTower with 1k buffers and 57600 kbaud
uart :: (BoardHSE p)
     => UART.UART
     -> Tower p ( ChannelSink   1024 (Stored Uint8)
                , ChannelSource 1024 (Stored Uint8))
uart u = UART.uartTower u 57600
