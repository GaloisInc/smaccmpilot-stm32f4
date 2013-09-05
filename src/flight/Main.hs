module Main where

import Ivory.Language

import Ivory.Stdlib.String (stdlibStringModule)
import qualified Ivory.Stdlib.SearchDir as Stdlib

import Ivory.Tower
import Ivory.Tower.Frontend
import qualified Ivory.HW.SearchDir as HW

import SMACCMPilot.Flight.Types (typeModules)

import SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import SMACCMPilot.Flight.Control (controlModules)

import SMACCMPilot.Flight.Control.Task
import SMACCMPilot.Flight.Motors.Task (motorMixerTask, px4ioarMotorDecoder)
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.UserInput.Task
import SMACCMPilot.Flight.BlinkTask
import SMACCMPilot.Flight.GCS.Tower

import qualified SMACCMPilot.Hardware.PX4IOAR as PX4IOAR

import SMACCMPilot.Param (paramModule)
import SMACCMPilot.Console (consoleModule)
import SMACCMPilot.Storage.Partition (partitionModule)

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import SMACCMPilot.Mavlink.Pack (packModule)
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)


import qualified Ivory.BSP.HWF4.EEPROM as HWF4
import qualified Ivory.BSP.HWF4.I2C as HWF4

import qualified Ivory.BSP.STM32F4.SearchDir as BSP
import qualified Ivory.BSP.STM32F4.GPIO as GPIO
import qualified Ivory.BSP.STM32F4.UART as UART

import Arm32SizeMap (sizeMap)

main :: IO ()
main = compile conf app
  where
  sp   = searchPathConf [Stdlib.searchDir, HW.searchDir, BSP.searchDir]
  conf = sp { bc_sizemap = Just sizeMap }

app :: Tower p ()
app = do
  (src_userinput, snk_userinput)   <- dataport
  (src_flightmode, snk_flightmode) <- dataport

  (_, snk_position)                <- dataport

  (src_motors,  snk_motors)        <- channel
  (src_sensors, snk_sensors)       <- channel
  (src_control, snk_control)       <- channel
  snk_motors_state                 <- stateProxy snk_motors
  snk_sensor_state                 <- stateProxy snk_sensors
  snk_control_state                <- stateProxy snk_control

  task "sensors"   $ sensorsTask src_sensors
  task "userInput" $ userInputTask src_userinput src_flightmode
  task "blink"     $ blinkTask [ relaypin, redledpin ] snk_flightmode
  task "control"   $ controlTask snk_flightmode snk_userinput
                      snk_sensors src_control
  task "motmix"    $ motorMixerTask snk_control snk_flightmode src_motors
  PX4IOAR.motorControlTower px4ioarMotorDecoder snk_motors

  gcsTower "uart1" UART.uart1 snk_flightmode snk_sensor_state snk_position
    snk_control_state snk_motors_state

  mapM_ addDepends typeModules
  mapM_ addModule otherms
  where
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
  -- Param and related helpers:
  , paramModule
  , consoleModule
  , partitionModule
  -- hwf4 bsp is used for I2C, EEPROM
  , HWF4.eepromModule
  , HWF4.i2cModule
  -- the rest:
  , userInputDecodeModule
  , stdlibStringModule
  ]
