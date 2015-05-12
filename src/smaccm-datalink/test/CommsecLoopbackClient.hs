
module Main where

import SMACCMPilot.Datalink.Client.Loopback
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Commsec.SymmetricKey

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO

import Ivory.Tower.Options
import Ivory.Tower.Config

main :: IO ()
main = do
  argv <- getArgs
  case getOpt' Permute options argv of
    (opts, [], tos, []) -> do
      let client_opts = foldl (flip id) defaultOpts opts
          topts = TOpts
            { topts_outdir = Nothing
            , topts_help   = False
            , topts_args   = tos
            , topts_error  = usage
            }
      sk <- getConfig topts symmetricKeyParser
      commsecLoopbackClient client_opts sk
    (_, nonOpts, unOpts, errs) -> usage ("invalid arguments: "
                                        ++ unwords nonOpts
                                        ++ unwords unOpts
                                        ++ unwords errs)

usage :: String -> IO a
usage errs = do
  hPutStr stderr (usageInfo errs options)
  exitFailure
