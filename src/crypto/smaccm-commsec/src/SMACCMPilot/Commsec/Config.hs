
module SMACCMPilot.Commsec.Config
  ( Config(..)
  , commsecConfigParser
  ) where

import Data.Word
import Tower.Config

data Config = Config
  { sendID     :: Word32   -- 0 - 15 uint32
  , sendKey    :: [Word8]  -- 16 uint8s
  , sendSalt   :: Word32   -- word32
  , recvKey    :: [Word8]  -- 16 uint8s
  , recvSalt   :: Word32   -- word32
  } deriving (Show, Read, Eq)

commsecConfigParser :: ConfigParser Config
commsecConfigParser = do
  sid <- subsection "sendID"  (boundedInteger 0 15)
  sk <- subsection "sendKey"  (arrayLen 16 (boundedInteger 0 255))
  ss <- subsection "sendSalt" (boundedInteger 0 (2 ^ 32 - 1))
  rk <- subsection "recvKey"  (arrayLen 16 (boundedInteger 0 255))
  rs <- subsection "recvSalt" (boundedInteger 0 (2 ^ 32 - 1))
  return Config
    { sendID   = sid
    , sendKey  = sk
    , sendSalt = ss
    , recvKey  = rk
    , recvSalt = rs
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
