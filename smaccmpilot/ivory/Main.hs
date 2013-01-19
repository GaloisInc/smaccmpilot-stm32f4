
module Main where

import Position
import Stabilize

import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = compile [ positionModule, stabilizeModule ]

