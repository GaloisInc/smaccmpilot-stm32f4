{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight
  ( flight
  , hil
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.Stdlib.String (stdlibStringModule)

import SMACCMPilot.Flight.Types (typeModules)
import SMACCMPilot.Flight.Control (controlModules)

import SMACCMPilot.Flight.Control.Task
import SMACCMPilot.Flight.Motors.Task
import SMACCMPilot.Flight.Motors.Platforms
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.Sensors.Platforms
import SMACCMPilot.Flight.UserInput.Task
import SMACCMPilot.Flight.BlinkTask
import SMACCMPilot.Flight.GCS.Tower
import SMACCMPilot.Flight.GCS.Transmit.MessageDriver (senderModules)
import SMACCMPilot.Flight.GPS

import SMACCMPilot.Console (consoleModule)

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import SMACCMPilot.Mavlink.Send (mavlinkSendModule)
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)
import SMACCMPilot.Mavlink.Pack (packModule)

import SMACCMPilot.Flight.Commsec.Commsec
import Ivory.HXStream

import qualified Ivory.BSP.HWF4.EEPROM as HWF4
import qualified Ivory.BSP.HWF4.I2C as HWF4

import qualified Ivory.BSP.STM32F4.GPIO as GPIO
import qualified Ivory.BSP.STM32F4.UART as UART
import           Ivory.BSP.STM32F4.RCC (BoardHSE(..))

hil :: (BoardHSE p, MotorOutput p, SensorOrientation p)
    => Tower p ()
hil = do
  -- Communication primitives:
  sensors       <- channel

  -- Instantiate core:
  (flightmode, control, motors) <- core (snk sensors)
  motors_state  <- stateProxy motors
  control_state <- stateProxy control

  -- HIL-enabled GCS on uart1:
  (istream, ostream) <- uart UART.uart1
  gcsTowerHil "uart1" istream ostream flightmode
    control_state motors_state sensors

flight :: (BoardHSE p, MotorOutput p, SensorOrientation p)
    => Tower p ()
flight = do
  -- Communication primitives:
  sensors       <- channel
  sensor_state  <- stateProxy (snk sensors)
  position      <- dataport

  -- Instantiate core:
  (flightmode, control, motors) <- core (snk sensors)
  motors_state  <- stateProxy motors
  control_state <- stateProxy control

  -- GPS Input on uart6 (valid for all px4fmu platforms)
  gps_position <- gpsTower UART.uart6
  -- Sensors managed by AP_HAL
  sensorsTower gps_position (src sensors)
  -- Motor output dependent on platform
  motorOutput motors

  -- GCS on UART1:
  (uart1istream, uart1ostream) <- uart UART.uart1
  gcsTower "uart1" uart1istream uart1ostream flightmode sensor_state
    (snk position) control_state motors_state

core :: (SingI n)
       => ChannelSink n (Struct "sensors_result")
       -> Tower p ( DataSink (Struct "flightmode")
                  , ChannelSink 16 (Struct "controloutput")
                  , ChannelSink 16 (Struct "motors"))
core sensors = do
  motors  <- channel
  control <- channel

  (userinput, flightmode) <- userInputTower
  task "blink"     $ blinkTask lights flightmode
  task "control"   $ controlTask flightmode userinput sensors (src control)
  task "motmix"    $ motorMixerTask (snk control) flightmode (src motors)

  mapM_ addDepends typeModules
  mapM_ addModule otherms

  return (flightmode, snk control, snk motors)
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
  ] ++
  [ packModule
  , mavlinkCRCModule
  , consoleModule
  -- hwf4 bsp is used for I2C, EEPROM
  , HWF4.eepromModule
  , HWF4.i2cModule
  -- the rest:
  , stdlibStringModule
  -- crypto
  , commsecModule
  -- hxstream
  , hxstreamModule

  , senderModules
  , mavlinkSendModule
  ]

-- Helper: a uartTower with 1k buffers and 57600 kbaud
uart :: (BoardHSE p)
     => UART.UART
     -> Tower p ( ChannelSink   1024 (Stored Uint8)
                , ChannelSource 1024 (Stored Uint8))
uart u = UART.uartTower u 57600
