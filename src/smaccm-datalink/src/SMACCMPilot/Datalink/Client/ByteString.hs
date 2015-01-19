module SMACCMPilot.Datalink.Client.ByteString where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString            as B
import           Text.Printf

import           SMACCMPilot.Datalink.Client.Monad

bytestringPad :: Integer -> ByteString -> GW ByteString
bytestringPad l bs =
  if B.length bs <= len
    then return $ bs `B.append` (B.pack $ replicate (len - B.length bs) 0)
    else writeErr "bytestringPad got oversized bytestring" >> return bs
  where
  len = fromInteger l

bytestringDebugger :: String -> ByteString -> GW ByteString
bytestringDebugger tag bs = writeDbg msg >> return bs
  where
  msg = printf "%s ByteString %d [%s]" tag (B.length bs) body
  body = fixup (unwords (map hexdig (B.unpack bs)))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse

bytestringDebugWhen ::
  (ByteString -> Bool) -> String -> ByteString -> GW ByteString
bytestringDebugWhen p tag bs = case p bs of
    True -> writeDbg msg >> return bs
    False -> return bs
  where
  msg = printf "%s ByteString %d [%s]" tag (B.length bs) body
  body = fixup (unwords (map hexdig (B.unpack bs)))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse

