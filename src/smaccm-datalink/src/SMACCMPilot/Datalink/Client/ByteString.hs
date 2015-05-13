module SMACCMPilot.Datalink.Client.ByteString where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString            as B
import           Data.List
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
  body = intercalate ", " (map hexdig (B.unpack bs))
  hexdig = printf "0x%0.2x"

bytestringDebug :: String -> ByteString -> String
bytestringDebug tag bs = printf "%s ByteString %s" tag (bytestringShowHex bs)
