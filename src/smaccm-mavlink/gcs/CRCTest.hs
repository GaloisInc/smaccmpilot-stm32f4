
module Main where

import qualified CanonicalCRC as Canonical
import GCS.Mavlink.CRC

import Data.Word
import Text.Printf

crc2words :: Word8 -> Word8 -> Word16
crc2words a b = crc_accumulate ( crc_accumulate crc_init_v a) b

main = mapM_ test Canonical.crc

test :: (Int, Int, Int) -> IO ()
test (a, b, expect) =
  let calc = crc2words (fromIntegral a) (fromIntegral b) in
  case calc == (fromIntegral expect) of
    True -> return ()
    False -> putStrLn $
      printf "failed: %d, %d -> %d, expected %d" a b calc expect

