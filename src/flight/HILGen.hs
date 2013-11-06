
module Main where

import qualified SMACCMPilot.Flight.Commsec.CommsecOpts as C
import           System.Environment
import           SMACCMPilot.Flight (hil)
import           SMACCMPilot.Flight.Compile (compile)

main :: IO ()
main = do
  args <- getArgs
  let (commsecOpts, nonOpts, unrecOpts) = C.getOpts args
  compile (hil commsecOpts) (nonOpts ++ unrecOpts)
