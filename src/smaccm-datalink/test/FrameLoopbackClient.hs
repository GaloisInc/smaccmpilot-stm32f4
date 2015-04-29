
module Main where

import SMACCMPilot.Datalink.Client.Loopback
import SMACCMPilot.Datalink.Client.Opts

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO

main :: IO ()
main = do
  argv <- getArgs
  case getOpt' Permute options argv of
    (opts, [], [], []) -> frameLoopbackClient (foldl (flip id) defaultOpts opts)
    (_, nonOpts, unOpts, errs) -> usage ("invalid arguments: "
                                        ++ unwords nonOpts
                                        ++ unwords unOpts
                                        ++ unwords errs)

usage :: String -> IO a
usage errs = do
  hPutStr stderr (usageInfo errs options)
  exitFailure
