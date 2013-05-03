
module CRC
  ( crc_accumulate
  , crc_lo_hi
  , crc_init_v
  ) where

import Data.Word
import Data.Bits

crc_accumulate :: Word16 -> Word8 -> Word16
crc_accumulate crc_start byte = v7
  where
  (crc_start_lo, _) = extractByte crc_start
  v0 = byte `xor` crc_start_lo
  v1 = v0 `shiftL` 4
  v2 = v0 `xor` v1
  v2_16 = extend v2
  v3 = v2_16 `shiftL` 8
  v4 = v2_16 `shiftL` 3
  v5 = v2_16 `shiftR` 4
  v6 = crc_start `shiftR` 8
  v7 = v6 `xor` v3 `xor` v4 `xor` v5

crc_lo_hi :: Word16 -> (Word8, Word8)
crc_lo_hi a = (lo, hi)
  where
  (lo, hi_16) = extractByte a
  (hi, _    ) = extractByte hi_16

crc_init_v :: Word16
crc_init_v = 0xFFFF

extend :: Word8 -> Word16
extend = fromIntegral

extractByte :: (Bits a, Integral a) => a -> (Word8, a)
extractByte a = (fromIntegral (a .&. 0xFF), a `shiftR` 8)

