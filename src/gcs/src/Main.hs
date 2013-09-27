module Main where

import qualified Data.ByteString as B

import           Data.Word
import           CommsecServer

--------------------------------------------------------------------------------

main :: IO ()
main = commsecServer True baseToUavKey b2uSalt uavToBaseKey u2bSalt

--------------------------------------------------------------------------------
-- Testing

uavToBaseKey, baseToUavKey :: B.ByteString
uavToBaseKey = B.pack [0..15    :: Word8]
baseToUavKey = B.pack [15,14..0 :: Word8]

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

--------------------------------------------------------------------------------

