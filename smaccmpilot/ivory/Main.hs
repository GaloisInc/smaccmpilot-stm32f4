
module Main where

import qualified PositionType as P
import qualified Stabilize as S
import qualified ServoType as R
import qualified MotorsOutputType as M

import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = compile [ P.positionModule
               , S.stabilizeModule
               , R.servoModule
               , M.motorsOutputModule ]

