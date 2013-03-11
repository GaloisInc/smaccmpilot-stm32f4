module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.Tower
import Ivory.Tower.Graphviz
import Ivory.Tower.Compile.FreeRTOS
import Ivory.Tower.Connections.FreeRTOS

import qualified Smaccm.Stm32f4.GPIO as GPIO

import SMACCMPilot.Flight.Types (typeModules)

import SMACCMPilot.Flight.GCS.TransmitDriver (gcsTransmitDriverModule)
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
import SMACCMPilot.Flight.Sensors.Task
import SMACCMPilot.Flight.UserInput.Task
import SMACCMPilot.Flight.BlinkTask

  -- for debugging, to remove:
import FooBarTasks

import Arm32SizeMap (sizeMap)

otherms :: [Module]
otherms = typeModules ++ controlModules ++
  [ gcsTransmitDriverModule
  , userInputDecodeModule
  , cstringModule
  , consoleModule
  , i2cModule
  , eepromModule
  , partitionModule
  , paramModule
  , gpioModule
  -- for debugging, to remove:
  , fooBarTypesModule
  ]

main :: IO ()
main = compileWithSizeMap sizeMap $ compileTower app

app :: Assembly
app = tower $ do
  (src_userinput, snk_userinput)   <- connector sharedState
  (src_sensors, snk_sensors)       <- connector sharedState
  (src_control, _)                 <- connector sharedState
  (src_flightmode, snk_flightmode) <- connector sharedState

  addTask $ sensorsTask src_sensors
  addTask $ userInputTask src_userinput src_flightmode
  addTask $ blinkTask GPIO.pin_b13 snk_flightmode
  addTask $ controlTask snk_flightmode snk_userinput snk_sensors src_control

  mapM_ addModule otherms

gviz :: IO ()
gviz = graphvizToFile "out.dot" app
