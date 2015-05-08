{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Datalink.Client.Opts
  ( Options(..)
  , options
  , defaultOpts
  , mkSerBaud
  , readMaybe
  ) where

import           System.Console.GetOpt
import           System.Posix.Terminal (BaudRate(..))

deriving instance Show BaudRate

data Options = Options
  { logLevel   :: Integer  -- Logging verbosity
  , serPort    :: Maybe FilePath -- Serial Port Filename
  , serBaud    :: BaudRate -- Serial Baud Rate
  } deriving (Show)

defaultOpts :: Options
defaultOpts = Options
  { logLevel = 1
  , serPort  = Nothing
  -- If you change default serBaud, also change integer num in options msg
  , serBaud  = B57600
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["quiet"]
      (NoArg (\opts -> opts { logLevel = 0 }))
      "No warning or error reporting"
  , Option [] ["verbose"]
      (NoArg (\opts -> opts { logLevel = 2 }))
      "Full logging output"
  , Option [] ["debug"]
      (NoArg (\opts -> opts { logLevel = 3 }))
      "Full debug output (higher than verbose)"
  , Option [] ["serial"]
      (ReqArg (\arg opts -> opts { serPort = Just arg }) "filename")
      ("Serial port filename (default: none)")
  , Option [] ["baud"]
      (ReqArg (\arg opts -> opts { serBaud = mkSerBaud arg }) "baudrate")
      ("Serial port baud rate (default:57600")
  ]

mkSerBaud :: String -> BaudRate
mkSerBaud opt = maybe (error "invalid serial baud rate") id (baudMaybe csint)
  where
  csint = maybe (error "Could not parse serial baud rate") id (readMaybe opt)
  baudMaybe :: Integer -> Maybe BaudRate
  baudMaybe 110    = Just B110
  baudMaybe 300    = Just B300
  baudMaybe 600    = Just B600
  baudMaybe 1200   = Just B1200
  baudMaybe 2400   = Just B2400
  baudMaybe 4800   = Just B4800
  baudMaybe 9600   = Just B9600
  baudMaybe 19200  = Just B19200
  baudMaybe 38400  = Just B38400
  baudMaybe 57600  = Just B57600
  baudMaybe 115200 = Just B115200
  baudMaybe _      = Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

