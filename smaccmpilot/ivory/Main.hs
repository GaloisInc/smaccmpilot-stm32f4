
module Main where

import PositionType      (positionModule)
import Stabilize         (stabilizeModule)
import ServoType         (servoModule)
import SensorsType       (sensorsTypeModule)
import MotorsOutputType  (motorsOutputModule)
import UserInputType     (userInputModule)
import GCSTransmitDriver (gcsTransmitDriverModule)
import UserInputDecode   (userInputDecodeModule)
import OptFlowType       (optFlowTypeModule)
import PositionEstimateType (positionEstimateTypeModule)

import IvoryCString               (cstringModule)

import SMACCMPilot.Driver.I2C     (i2cModule)
import SMACCMPilot.Storage.EEPROM (eepromModule)
import SMACCMPilot.Param          (paramModule)

import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = compile [ positionModule
               , stabilizeModule
               , servoModule
               , sensorsTypeModule
               , motorsOutputModule
               , userInputModule
               , gcsTransmitDriverModule
               , userInputDecodeModule
               , optFlowTypeModule
               , positionEstimateTypeModule
               , cstringModule
               , i2cModule
               , eepromModule
               , paramModule
               ]

