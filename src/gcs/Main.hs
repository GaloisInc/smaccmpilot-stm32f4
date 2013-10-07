
module Main where

import           System.Environment
import qualified Data.ByteString as B

import           Data.Word
import           Commsec.CommsecServer
import           Commsec.CommsecOpts

--------------------------------------------------------------------------------

main :: IO ()
main =
  --True baseToUavKey b2uSalt uavToBaseKey u2bSalt
  commsecServer . (\(opts, _, _) -> opts) . getOpts =<< getArgs

--------------------------------------------------------------------------------
-- Testing

uavToBaseKey, baseToUavKey :: B.ByteString
uavToBaseKey = B.pack [0..15    :: Word8]
baseToUavKey = B.pack [15,14..0 :: Word8]

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

--------------------------------------------------------------------------------

