
module SMACCMPilot.Commsec.Native.Opts
  ( Options(..)
  , options
  , defaultOpts
  ) where

import Data.Word
import System.Console.GetOpt

data Options = Options
  { sendID     :: Word32   -- 0 - 15 uint32
  , sendKey    :: [Word8]  -- 16 uint8s
  , sendSalt   :: Word32   -- word32
  , recvKey    :: [Word8]  -- 16 uint8s
  , recvSalt   :: Word32   -- word32
  } deriving (Show, Read, Eq)

defaultOpts :: Options
defaultOpts = Options
  { sendID   = 0
  , sendKey  = []
  , sendSalt = 0
  , recvKey  = []
  , recvSalt = 0
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

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

