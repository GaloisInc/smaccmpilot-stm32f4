module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.Tower
import Ivory.Tower.Compile.FreeRTOS
import Ivory.Tower.Connections.FreeRTOS

import PositionType      (positionModule)
import Stabilize         (stabilizeModule)
import ServoType         (servoModule)
import SensorsType       (sensorsTypeModule)
import MotorsOutputType  (motorsOutputModule)
import UserInputType     (userInputTypeModule)
import UserInputDecode   (userInputDecodeModule)
import GCSTransmitDriver (gcsTransmitDriverModule)
import OptFlowType       (optFlowTypeModule)
import PositionEstimateType (positionEstimateTypeModule)

import IvoryCString               (cstringModule)

import SMACCMPilot.Console           (consoleModule)
import SMACCMPilot.Driver.I2C        (i2cModule)
import SMACCMPilot.Storage.EEPROM    (eepromModule)
import SMACCMPilot.Storage.Partition (partitionModule)
import SMACCMPilot.Param             (paramModule)

import UserInputTask
import FooBarTasks

import Arm32SizeMap (sizeMap)

otherms :: [Module]
otherms =
  [ positionModule
  , stabilizeModule
  , servoModule
  , sensorsTypeModule
  , motorsOutputModule
  , gcsTransmitDriverModule
  , userInputDecodeModule
  , optFlowTypeModule
  , positionEstimateTypeModule
  , cstringModule
  , consoleModule
  , i2cModule
  , eepromModule
  , partitionModule
  , paramModule
  , fooBarTypesModule
  , userInputTypeModule
  ]

assembly :: Assembly
assembly = tower $ do
  (src_foo, sink_foo) <- connector sharedState
  (src_bar, sink_bar) <- connector sharedState
  (src_userinput, _)  <- connector sharedState

  addTask (fooSource src_foo)
  addTask (barSource src_bar)
  addTask (fooBarSink sink_foo sink_bar)

  addTask (userInputTask src_userinput)

  mapM_ addModule otherms

main :: IO ()
main = compile $ compileTower assembly
-- main = compileWithSizeMap sizeMap $ towerModules ++
--   [ positionModule
--   , stabilizeModule
--   , servoModule
--   , sensorsTypeModule
--   , motorsOutputModule
--   , userInputModule
--   , gcsTransmitDriverModule
--   , userInputDecodeModule
--   , optFlowTypeModule
--   , positionEstimateTypeModule
--   , cstringModule
--   , consoleModule
--   , i2cModule
--   , eepromModule
--   , partitionModule
--   , paramModule
--   ]
