
module Main where

import SMACCMPilot.Flight (flight)
import SMACCMPilot.Flight.Compile (compile)

main :: IO ()
main = compile flight
