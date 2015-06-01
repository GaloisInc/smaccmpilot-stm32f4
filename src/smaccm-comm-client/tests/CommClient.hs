
module Main where

import Ivory.Tower.Options
import Ivory.Tower.Config
import SMACCMPilot.Comm.Native.Client
import SMACCMPilot.Comm.Native.Client.Opts
import SMACCMPilot.Datalink.Mode

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO

main :: IO ()
main = do
  argv <- getArgs
  case getOpt' Permute clientoptions argv of
    (opts, [], tos, []) -> do
      let client_opts = foldl (flip id) defaultClientOpts opts
          topts = TOpts
            { topts_outdir = Nothing
            , topts_help   = False
            , topts_args   = tos
            , topts_error  = usage
            }
      cmode <- getConfig topts $ datalinkModeParser DatalinkClient
      commClient client_opts cmode
    (_, nonOpts, unOpts, errs) -> usage ("invalid arguments: "
                                        ++ unwords nonOpts
                                        ++ unwords unOpts
                                        ++ unwords errs)

usage :: String -> IO a
usage errs = do
  hPutStr stderr (usageInfo errs clientoptions)
  exitFailure
