{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Comm.Native.Client.Opts
  ( ClientOptions(..)
  , clientoptions
  , defaultClientOpts
  ) where

import           System.Console.GetOpt
import           SMACCMPilot.Datalink.Client.Opts

data ClientOptions = ClientOptions
  { dlOpts     :: Options
  , srvPort    :: Int  -- Server TCP Port
  } deriving (Show)

defaultClientOpts :: ClientOptions
defaultClientOpts = ClientOptions
  { dlOpts   = defaultOpts
  , srvPort  = 8080
  }

clientoptions :: [OptDescr (ClientOptions -> ClientOptions)]
clientoptions =
  [ Option [] ["quiet"]
      (NoArg (\opts -> opts { dlOpts = (dlOpts opts) { logLevel = 0 }}))
      "No warning or error reporting"
  , Option [] ["verbose"]
      (NoArg (\opts -> opts { dlOpts = (dlOpts opts) { logLevel = 2 }}))
      "Full logging output"
  , Option [] ["debug"]
      (NoArg (\opts -> opts { dlOpts = (dlOpts opts) { logLevel = 3 }}))
      "Full debug output (higher than verbose)"
  , Option [] ["serial"]
      (ReqArg (\arg opts -> opts { dlOpts = (dlOpts opts) { serPort = Just arg }}) "filename")
      ("Serial port filename (default: none)")
  , Option [] ["baud"]
      (ReqArg (\arg opts -> opts { dlOpts = (dlOpts opts) { serBaud = mkSerBaud arg }}) "baudrate")
      ("Serial port baud rate (default: "
       ++ (drop 1 (show (serBaud (dlOpts defaultClientOpts))))
       ++ ")")
  , Option [] ["port"]
      (ReqArg (\arg opts -> opts { srvPort = mkSrvPort arg }) "portnumber")
      ("Server TCP port (default: " ++ (show (srvPort defaultClientOpts)) ++ ")")
  ]

mkSrvPort :: String -> Int
mkSrvPort opt =
  maybe (error "Could not parse server port") id (readMaybe opt)

