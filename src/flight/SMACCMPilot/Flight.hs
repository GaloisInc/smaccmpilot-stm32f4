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
  sensors       <- dataport
  fm_mavcmd     <- dataport
  armed_mavcmd  <- channel
  rc_override   <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
                      { sensors_in = snk sensors
                      , params_in = sysFlightParams snk_params
                      , rcoverride_in = snk rc_override
                      , armed_mavcmd_in = snk armed_mavcmd
                      , fm_mavcmd_in = snk fm_mavcmd
                      }

  control_state     <- stateProxy "control_state" (control_out core_out)
  motors_state      <- stateProxy "motors_state" (motors_out core_out)

  -- HIL-enabled GCS on uart1:
  (istream, ostream) <- uart UART.uart1

  gcsTowerHil "uart1" opts istream ostream
    (flightmode_state core_out)
    (src fm_mavcmd)
    (armed_state core_out)
    (src armed_mavcmd)
    control_state
    motors_state
    sensors
    (src rc_override)
    (altctl_state core_out)
    (userinput_state core_out)
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
  sensors       <- dataport
  armed_mavcmd  <- channel
  fm_mavcmd     <- dataport
  rc_override   <- channel

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- Instantiate core:
  core_out <- core $ FlightCoreRequires
                      { sensors_in = snk sensors
                      , params_in = sysFlightParams snk_params
                      , rcoverride_in = snk rc_override
                      , armed_mavcmd_in = snk armed_mavcmd
                      , fm_mavcmd_in = snk fm_mavcmd
                      }

  control_state     <- stateProxy "control_state" (control_out core_out)
  motors_state      <- stateProxy "motors_state" (motors_out core_out)

  -- GPS Input on uart6 (valid for all px4fmu platforms)
  gps_position <- gpsTower UART.uart6
  position_state <- stateProxy "position_state" gps_position
  -- Sensors managed by AP_HAL
  sensorsTower gps_position (src sensors)
  -- Motor output dependent on platform
  motorOutput (motors_out core_out)

  let gcsTower' uartNm uartiStrm uartoStrm =
        gcsTower uartNm opts uartiStrm uartoStrm
          (flightmode_state core_out)
          (src fm_mavcmd)
          (armed_state core_out)
          (src armed_mavcmd)
          (snk sensors)
          position_state
          control_state
          motors_state
          (src rc_override)
          (altctl_state core_out)
          (userinput_state core_out)
          paramList

  -- GCS on UART1:
  (uart1istream, uart1ostream) <- uart UART.uart1
  gcsTower' "uart1" uart1istream uart1ostream

  -- GCS on UART5:
  (uart5istream, uart5ostream) <- uart UART.uart5
  gcsTower' "uart5" uart5istream uart5ostream

  addModule (commsecModule opts)

-- Helper: a uartTower with 1k buffers and 57600 kbaud
uart :: (BoardHSE p)
     => UART.UART
     -> Tower p ( ChannelSink   1024 (Stored Uint8)
                , ChannelSource 1024 (Stored Uint8))
uart u = UART.uartTower u 57600
