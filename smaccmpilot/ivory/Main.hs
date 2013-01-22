
module Main where

import qualified PositionType as P
import qualified Stabilize as Stab
import qualified ServoType as Serv
import qualified SensorsType as Sens
import qualified MotorsOutputType as M
import qualified UserInputType as U
import qualified GCSTransmit as G

import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = compile [ P.positionModule
               , Stab.stabilizeModule
               , Serv.servoModule
               , Sens.sensorsTypeModule
               , M.motorsOutputModule
               , U.userInputModule
               , G.gcsTransmitModule ]

