
module Main where

import SMACCMPilot.Flight (hil)
import SMACCMPilot.Flight.Compile (compile)

main :: IO ()
main = compile hil
