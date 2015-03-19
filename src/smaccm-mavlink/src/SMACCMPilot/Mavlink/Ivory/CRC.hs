{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Mavlink.Ivory.CRC where

import Ivory.Language

mavlinkCRCModule :: Module
mavlinkCRCModule = package "mavlink_crc" $ do
  incl crc_accumulate

type CRC = Uint16

crc_accumulate :: Def ('[ Uint8 , Ref s (Stored CRC) ] :-> ())
crc_accumulate = proc "mavlink_crc_accumulate" $ \ byte crc -> body $ do
  crc_start <- deref crc
  let (crc_start_lo, _) = extractByte crc_start
  v0 :: Uint8  <- assign $ byte .^ crc_start_lo
  v1 :: Uint8  <- assign $ v0 `iShiftL` 4
  v2 :: Uint8  <- assign $ v0 .^ v1
  let v2_16 :: Uint16 = safeCast v2
  v3 :: Uint16 <- assign $ v2_16 `iShiftL` 8
  v4 :: Uint16 <- assign $ v2_16 `iShiftL` 3
  v5 :: Uint16 <- assign $ v2_16 `iShiftR` 4
  v6 :: Uint16 <- assign $ crc_start `iShiftR` 8
  v7 :: Uint16 <- assign $ v6 .^ v3 .^ v4 .^ v5
  store crc v7
  retVoid

crc_lo_hi :: Ref s (Stored CRC) -> Ivory eff (Uint8, Uint8)
crc_lo_hi crc = do
  val <- deref crc
  let (lo, hi_16) = extractByte val
  let (hi, _)     = extractByte hi_16
  return (lo, hi)


crc_init_v :: CRC
crc_init_v = 0xFFFF -- X25_INIT_CRC

