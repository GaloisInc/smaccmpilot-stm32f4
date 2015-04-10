{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Commsec.Keys
  ( SymmetricKey(..)
  , client_encode_ks
  , client_decode_ks
  , server_encode_ks
  , server_decode_ks
  , KeySalt(..), ks_key, ks_salt
  , symmetricKeyParser
  ) where

import Data.Word (Word64, Word8)
import Data.Bits (shiftL)
import Ivory.Tower.Config

data SymmetricKey = SymmetricKey
  { c2s_ks    :: KeySalt -- Client to Server
  , s2c_ks    :: KeySalt -- Server to Client
  } deriving (Eq, Show)

-- By convention: Client is a command and control station
-- Server is a vehicle
client_encode_ks :: SymmetricKey -> KeySalt
client_encode_ks = c2s_ks
client_decode_ks :: SymmetricKey -> KeySalt
client_decode_ks = s2c_ks

server_encode_ks :: SymmetricKey -> KeySalt
server_encode_ks = s2c_ks
server_decode_ks :: SymmetricKey -> KeySalt
server_decode_ks = c2s_ks

data KeySalt = KeySalt
  { ks_keysalt :: [Word8]  -- 28 uint8s, first 16 == key, next 12 == salt
  } deriving (Eq, Show)

ks_key :: KeySalt -> [Word8]
ks_key = take 16 . ks_keysalt

-- Convert the last 12 bytes into a big-endian integer
ks_salt :: KeySalt -> Word64
ks_salt = foldl (\a b -> fromIntegral b + (a `shiftL` 8)) 0 . drop 16 . ks_keysalt

symmetricKeyParser :: ConfigParser SymmetricKey
symmetricKeyParser = subsection "symmetric_key" $ do
  s2c_ks <- subsection "server_to_client" ks
  c2s_ks <- subsection "client_to_server" ks
  return SymmetricKey { .. }
  where
  ks = KeySalt `fmap` subsection "keysalt" (arrayLen 24 (boundedInteger 0 255))
  arrayLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")
