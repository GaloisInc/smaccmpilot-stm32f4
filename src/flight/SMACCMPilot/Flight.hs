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

import Ivory.Stdlib (stdlibModules)

import qualified SMACCMPilot.Flight.Types.Armed as A
import SMACCMPilot.Flight.Types (typeModules)
import SMACCMPilot.Flight.Control (controlModules)
import SMACCMPilot.Flight.Commsec.Commsec (commsecModule)

import SMACCMPilot.Flight.Control.Task
import SMACCMPilot.Flight.Motors.Task
import SMACCMPilot.Flight.Motors.Platforms
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.Sensors.Platforms
import SMACCMPilot.Flight.UserInput.Tower
import SMACCMPilot.Flight.BlinkTask
import SMACCMPilot.Flight.GCS.Tower
import SMACCMPilot.Flight.GCS.Transmit.MessageDriver (senderModules)
import SMACCMPilot.Flight.GPS
import SMACCMPilot.Flight.Param

import SMACCMPilot.Param

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import SMACCMPilot.Mavlink.Send (mavlinkSendModule)
import SMACCMPilot.Mavlink.Receive (mavlinkReceiveStateModule)
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)
import SMACCMPilot.Mavlink.Pack (packModule)

import SMACCMPilot.Hardware.GPS.Types (gpsTypesModule)

import qualified SMACCMPilot.Flight.Commsec.CommsecOpts as C
import Ivory.HXStream

import qualified Ivory.BSP.STM32F4.GPIO as GPIO
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
  flightmode    <- dataport
  -- Arming input from GCS.  Goes to MAVLink mux.
  armed_mav     <- channel
  -- Result of the MAVLink/PPM mux.  Goes to control & GCS TX.
  armed_res     <- dataport

  -- RC override channel
  (rcOvrTx, rcOvrRx) <- channel

  -- alt hold state debugging
  ah_state <- dataport

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- Instantiate core:
  let flightparams = sysFlightParams snk_params
  (control, motors) <- core (snk sensors)
                            flightmode
                            armed_res
                            (snk armed_mav)
                            flightparams
                            rcOvrRx
                            (src ah_state)
  motors_state      <- stateProxy motors
  control_state     <- stateProxy control

  -- HIL-enabled GCS on uart1:
  (istream, ostream) <- uart UART.uart1

  gcsTowerHil "uart1" opts istream ostream flightmode
    (snk armed_res) (src armed_mav) control_state motors_state
    sensors rcOvrTx (snk ah_state) paramList

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
  let sensor_state = snk sensors
  flightmode    <- dataport
  -- Arming input from GCS.  Goes to MAVLink mux.
  armed_mav     <- channel
  -- Result of the MAVLink/PPM mux.  Goes to control & GCS TX.
  armed_res     <- dataport

  -- RC override channel:
  (rcOvrTx, rcOvrRx) <- channel

  -- alt hold state debugging
  ah_state <- dataport

  -- Parameters:
  (params, paramList) <- initTowerParams sysParams
  let snk_params       = portPairSink <$> params

  -- Instantiate core:
  let flightparams = sysFlightParams snk_params
  (control, motors) <- core (snk sensors)
                            flightmode
                            armed_res
                            (snk armed_mav)
                            flightparams
                            rcOvrRx
                            (src ah_state)
  motors_state      <- stateProxy motors
  control_state     <- stateProxy control

  -- GPS Input on uart6 (valid for all px4fmu platforms)
  gps_position <- gpsTower UART.uart6
  position_state <- stateProxy gps_position
  -- Sensors managed by AP_HAL
  sensorsTower gps_position (src sensors)
  -- Motor output dependent on platform
  motorOutput motors

  let gcsTower' uartNm uartiStrm uartoStrm =
        gcsTower uartNm opts uartiStrm uartoStrm flightmode
          (snk armed_res) (src armed_mav) sensor_state position_state
          control_state motors_state rcOvrTx (snk ah_state) paramList

  -- GCS on UART1:
  (uart1istream, uart1ostream) <- uart UART.uart1
  gcsTower' "uart1" uart1istream uart1ostream

  -- GCS on UART5:
  (uart5istream, uart5ostream) <- uart UART.uart5
  gcsTower' "uart5" uart5istream uart5ostream

  addModule (commsecModule opts)

core :: (SingI n1, SingI n2)
       => DataSink (Struct "sensors_result")
       -> ( DataSource (Struct "flightmode")
          , DataSink (Struct "flightmode"))
       -> ( DataSource (Stored A.ArmedMode)
          , DataSink   (Stored A.ArmedMode))
       -> ChannelSink n1 (Stored A.ArmedMode)
       -> FlightParams ParamSink
       -> ChannelSink n2 (Struct "rc_channels_override_msg")
       -> DataSource (Struct "alt_hold_state")
       -> Tower p ( ChannelSink 16 (Struct "controloutput")
                  , ChannelSink 16 (Struct "motors"))
core sensors flightmode armed_res armed_mav_snk
     flightparams snk_rc_override_msg ah_state_src
  = do
  motors  <- channel
  control <- channel
  let (src_flightmode, snk_flightmode) = flightmode

  userinput        <- userInputTower armed_res armed_mav_snk
                          snk_rc_override_msg src_flightmode
  task "blink"      $ blinkTask lights (snk armed_res) (snk_flightmode)
  task "control"    $ controlTask (snk armed_res) (snk_flightmode) userinput sensors
                       (src control) ah_state_src flightparams
  task "motmix"     $ motorMixerTask (snk control) (snk armed_res)
                        (snk_flightmode) (src motors)

  mapM_ addDepends typeModules
  mapM_ addModule otherms

  return (snk control, snk motors)
  where
  lights = [relaypin, redledpin]
  relaypin = GPIO.pinB13
  redledpin = GPIO.pinB14

otherms :: [Module]
otherms = concat
  -- flight types
  [ typeModules
  -- control subsystem
  , controlModules
  -- mavlink system
  , mavlinkMessageModules
  -- standard library
  , stdlibModules
  ] ++
  [ packModule
  , mavlinkCRCModule
  , paramModule
  -- the rest:
  -- hxstream
  , hxstreamModule

  , senderModules
  , mavlinkSendModule
  , mavlinkReceiveStateModule
  ]

-- Helper: a uartTower with 1k buffers and 57600 kbaud
uart :: (BoardHSE p)
     => UART.UART
     -> Tower p ( ChannelSink   1024 (Stored Uint8)
                , ChannelSource 1024 (Stored Uint8))
uart u = UART.uartTower u 57600
