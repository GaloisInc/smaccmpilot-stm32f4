module SMACCMPilot.Datalink.Client.ByteString where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString            as B
import           Text.Printf

bytestringPad :: Integer -> ByteString -> Either String ByteString
bytestringPad l bs
  | B.length bs <= len =
      Right $ bs `B.append` (B.pack $ replicate (len - B.length bs) 0)
  | otherwise =
      Left "bytestringPad got oversized bytestring"
  where
  len = fromInteger l

bytestringShowHex :: ByteString -> String
bytestringShowHex bs = printf "[%s](%d)" body (B.length bs)
  where
  body = fixup (unwords (map hexdig (B.unpack bs)))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse

bytestringDebug :: String -> ByteString -> String
bytestringDebug tag bs = printf "%s ByteString %s" tag (bytestringShowHex bs)
