module SMACCMPilot.GCS.Commsec.Test (main) where

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

--import Crypto.Cipher.AES                -- Vincent's GCM routine
--import Crypto.Cipher.Types
--import Data.IORef
--import Data.Serialize
import SMACCMPilot.GCS.Commsec

maxMsgLen :: Int
maxMsgLen = 84

uavID, base0ID  :: BaseId
uavID   = 0
base0ID = 0
-- base1ID = 1

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

someMsg :: String
someMsg = "This is a message from "

mkMsg :: String -> String
mkMsg msg = m ++ replicate n ' '
  where m = someMsg ++ msg
        n = maxMsgLen - length m

uavToBaseKey, baseToUavKey :: ByteString
uavToBaseKey = B.pack [0..15::Word8]
baseToUavKey = B.pack [15,14..0::Word8]

main :: IO ()
main = do
  uavCtx   <- secPkgInit_HS uavID   b2uSalt baseToUavKey u2bSalt uavToBaseKey
  base0Ctx <- secPkgInit_HS base0ID u2bSalt uavToBaseKey b2uSalt baseToUavKey
  -- base1Ctx <- secPkgInit_HS base1ID u2bSalt uavToBaseKey b2uSalt baseToUavKey

  Just uavPkg <- secPkgEncInPlace_HS uavCtx (BC.pack $ mkMsg "uav!")

  putStrLn (simpleHex uavPkg)

  secPkgDec_HS base0Ctx uavPkg >>= report
  -- Can't decrypt twice.
  -- secPkgDec_HS base0Ctx uavPkg >>= report
  where
  report (Left err) = putStrLn ("failure: " ++ (show err))
  report (Right res) = BC.putStrLn res

simpleHex :: a -> String
simpleHex = const "" -- XXX where did lee get this function from (module Hexdump)?
