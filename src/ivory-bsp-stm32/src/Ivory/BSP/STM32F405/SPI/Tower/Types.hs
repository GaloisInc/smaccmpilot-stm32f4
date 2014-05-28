{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32F405.SPI.Tower.Types where

import Ivory.Language
import Ivory.BSP.STM32F405.SPI.Tower.Types.SPIDeviceHandle

[ivory|
struct spi_transaction_request
  { tx_device :: Stored SPIDeviceHandle
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  }
|]

[ivory|
struct spi_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  ; rx_idx     :: Stored (Ix 128)
  }
|]

spiTowerTypes :: Module
spiTowerTypes = package "spiTowerTypes" $ do
  defStruct (Proxy :: Proxy "spi_transaction_request")
  defStruct (Proxy :: Proxy "spi_transaction_result")

