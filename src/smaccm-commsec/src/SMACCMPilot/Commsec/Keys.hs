{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Commsec.Keys
  ( SymmetricKey(..)
  , client_encode_ks
  , client_decode_ks
  , server_encode_ks
  , server_decode_ks
  , KeySalt(..)
  , symmetricKeyParser
  ) where

import Data.Word
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
  { ks_key  :: [Word8]  -- 16 uint8s
  , ks_salt :: Word32   -- word32
  } deriving (Eq, Show)

symmetricKeyParser :: ConfigParser SymmetricKey
symmetricKeyParser = subsection "symmetric_key" $ do
  s2c_ks <- subsection "server_to_client" ks
  c2s_ks <- subsection "client_to_server" ks
  return SymmetricKey { .. }
  where
  ks = do
    k <- subsection "key" (arrayLen 16 (boundedInteger 0 255))
    s <- subsection "salt" (boundedInteger 0 ((2 ^ (32::Integer)) - 1))
    return KeySalt { ks_key = k, ks_salt = s }
  arrayLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")
