
module SMACCMPilot.Commsec.Config
  ( Config(..)
  , configParser
  ) where

import Data.Word

data Config = Config
  { sendID     :: Word32   -- 0 - 15 uint32
  , sendKey    :: [Word8]  -- 16 uint8s
  , sendSalt   :: Word32   -- word32
  , recvKey    :: [Word8]  -- 16 uint8s
  , recvSalt   :: Word32   -- word32
  } deriving (Show, Read, Eq)

configParser :: a -> Config -- not really tho
configParser = undefined

