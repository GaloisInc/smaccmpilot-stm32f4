
module SMACCMPilot.Commsec.Config
  ( Config(..)
  , commsecConfigParser
  ) where

import Data.Word
import Tower.Config

data Config = Config
  { encode_id     :: Word32   -- 0 - 15 uint32
  , encode_key    :: [Word8]  -- 16 uint8s
  , encode_salt   :: Word32   -- word32
  , decode_key    :: [Word8]  -- 16 uint8s
  , decode_salt   :: Word32   -- word32
  } deriving (Show, Read, Eq)

commsecConfigParser :: ConfigParser Config
commsecConfigParser = do
  ei <- subsection "encode_id"  (boundedInteger 0 15)
  ek <- subsection "encode_key"  (arrayLen 16 (boundedInteger 0 255))
  es <- subsection "encode_salt" (boundedInteger 0 ((2 ^ (32::Integer)) - 1))
  dk <- subsection "decode_key"  (arrayLen 16 (boundedInteger 0 255))
  ds <- subsection "decode_salt" (boundedInteger 0 ((2 ^ (32::Integer)) - 1))
  return Config
    { encode_id   = ei
    , encode_key  = ek
    , encode_salt = es
    , decode_key  = dk
    , decode_salt = ds
    }
  where
  arrayLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")
