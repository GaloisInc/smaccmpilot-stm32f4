module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.Tower
import Ivory.Tower.Graphviz
import Ivory.Tower.Compile.FreeRTOS
import Ivory.Tower.Connections.FreeRTOS

import qualified Smaccm.Stm32f4.GPIO as GPIO

import SMACCMPilot.Flight.Types (typeModules)

import SMACCMPilot.Flight.UserInput.Decode (userInputDecodeModule)
import SMACCMPilot.Flight.Control (controlModules)

import SMACCMPilot.Console           (consoleModule)
import SMACCMPilot.Driver.I2C        (i2cModule)
import SMACCMPilot.Driver.Gpio       (gpioModule)
import SMACCMPilot.Storage.EEPROM    (eepromModule)
import SMACCMPilot.Storage.Partition (partitionModule)
import SMACCMPilot.Param             (paramModule)
import SMACCMPilot.Util.IvoryCString (cstringModule)

import SMACCMPilot.Flight.Control.Task
import SMACCMPilot.Flight.Motors.Task
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.UserInput.Task
import SMACCMPilot.Flight.BlinkTask
import SMACCMPilot.Flight.GCS.Transmit.Task

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import SMACCMPilot.Mavlink.Pack (packModule)

import Arm32SizeMap (sizeMap)

otherms :: [Module]
otherms = 
  -- flight types
  typeModules ++
  -- control subsystem
  controlModules ++
  -- mavlink system
  mavlinkMessageModules ++ [packModule] ++
  -- the rest:
  [ userInputDecodeModule
  , cstringModule
  , consoleModule
  , i2cModule
  , eepromModule
  , partitionModule
  , paramModule
  , gpioModule
  ]

main :: IO ()
main = do compileWithSizeMap sizeMap $ compileTower app
          gviz

app :: Assembly
app = tower $ do
  (src_userinput, snk_userinput)   <- connector sharedState
  (src_sensors, snk_sensors)       <- connector sharedState
  (src_control, snk_control)       <- connector sharedState
  (src_flightmode, snk_flightmode) <- connector sharedState
  (src_servos, _)                  <- connector sharedState

  addTask $ sensorsTask src_sensors
  addTask $ userInputTask src_userinput src_flightmode
  addTask $ blinkTask GPIO.pin_b13 snk_flightmode
  addTask $ controlTask snk_flightmode snk_userinput snk_sensors src_control
  addTask $ motorsTask snk_control snk_flightmode src_servos

  addTask $ gcsTransmitTask snk_flightmode

  mapM_ addModule otherms

gviz :: IO ()
gviz = graphvizToFile "out.dot" app
