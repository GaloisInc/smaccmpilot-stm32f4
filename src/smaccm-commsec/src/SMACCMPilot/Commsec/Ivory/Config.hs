
module SMACCMPilot.Commsec.Ivory.Config
  ( Config(..)
  , KeySalt(..)
  , commsecConfigParser
  ) where

import Data.Word
import Tower.Config

data Config = Config
  { encode_id :: Integer -- 0 - 15
  , encode_ks :: KeySalt
  , decode_ks :: KeySalt
  } deriving (Eq, Show)

data KeySalt = KeySalt
  { ks_key  :: [Word8]  -- 16 uint8s
  , ks_salt :: Word32   -- word32
  } deriving (Eq, Show)

commsecConfigParser :: ConfigParser Config
commsecConfigParser = do
  ei  <- subsection "encode" (subsection "id" (boundedInteger 0 15))
  eks <- subsection "encode" ks
  dks <- subsection "decode" ks
  return Config
    { encode_id = ei
    , encode_ks = eks
    , decode_ks = dks
    }
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
