-- | Parse options for commsec communication.

module Commsec.CommsecOpts where

import Data.Word
import System.Console.GetOpt

data Options = Options
  { sendID     :: Word32   -- 0 - 15 uint32
  , sendKey    :: [Word8]  -- 16 uint8s
  , sendSalt   :: Word32   -- word32
  , recvKey    :: [Word8]  -- 16 uint8s
  , recvSalt   :: Word32   -- word32
  , logLevel   :: Integer  -- Logging verbosity
  , serPort    :: FilePath -- Serial Port Filename
  , serBaud    :: Integer  -- Serial Baud Rate
  , srvPort    :: Integer  -- Server TCP Port
  } deriving (Show, Read, Eq)

defaultOpts :: Options
defaultOpts = Options
  { sendID   = 0
  , sendKey  = []
  , sendSalt = 0
  , recvKey  = []
  , recvSalt = 0
  , logLevel = 1
  , serPort  = "/dev/ttyUSB0"
  , serBaud  = 57600
  , srvPort  = 6000
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["senderid"]
      (ReqArg (\arg opts -> opts { sendID = mkID arg}) "uint32")
      "sender identifier"
  , Option [] ["sendkey"]
      (ReqArg (\arg opts -> opts { sendKey = mkKey arg}) "[uint8]")
      "sender key: 16 uint8s in form '[3, 4, 5, ..., n]'"
  , Option [] ["sendsalt"]
      (ReqArg (\arg opts -> opts { sendSalt = mkSalt arg}) "uint32")
      "sender salt"
  , Option [] ["recvkey"]
      (ReqArg (\arg opts -> opts { recvKey = mkKey arg}) "[uint8]")
      "recevier key: 16 uint8s in form '[3, 4, 5, ..., n]'"
  , Option [] ["recvsalt"]
      (ReqArg (\arg opts -> opts { recvSalt = mkSalt arg}) "uint32")
      "receiver salt"
  , Option [] ["quiet"]
      (NoArg (\opts -> opts { logLevel = 0 }))
      "No warning or error reporting"
  , Option [] ["verbose"]
      (NoArg (\opts -> opts { logLevel = 2 }))
      "Full debug output"
  , Option [] ["serial"]
      (ReqArg (\arg opts -> opts { serPort = arg }) "filename")
      "Serial port filename (default: /dev/ttyUSB0)"
  , Option [] ["baud"]
      (ReqArg (\arg opts -> opts { serBaud = (read arg) }) "baudrate")
      "Serial port baud rate (default: 57600)"
  , Option [] ["port"]
      (ReqArg (\arg opts -> opts { srvPort = (read arg) }) "portnumber")
      "Server TCP port (default: 6000)"
  ]

mkID :: String -> Word32
mkID opt =
  maybe (error "Could not parse Sender ID") chkSz (readMaybe opt)
  where
  chkSz ident | ident < 16 = ident
              | otherwise  = error "SenderID must be less than 16"

mkSalt :: String -> Word32
mkSalt opt =
  maybe (error "Could not parse salt") id (readMaybe opt)

mkKey :: String -> [Word8]
mkKey opt =
  maybe (error "Could not parse key") chkLen (readMaybe opt)
  where
  chkLen ls | length ls == 16 = ls
            | otherwise       = error "Key wrong length: expected length: 16."

getOpts :: [String] -> (Options, [String], [String])
getOpts argv = do
  case getOpt' Permute options argv of
    (opts, nonOpts, unrecOpts, errs)
      | not (null errs)    -> error $ "Option errors: "
                                   ++ unwords errs ++ usageInfo "" options
      | otherwise          -> ( foldl (flip id) defaultOpts opts
                              , nonOpts
                              , unrecOpts
                              )

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing
