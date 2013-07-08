{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPITypes where

import Ivory.Language

[ivory|
struct spi_transmission
  { tx_buf  :: Array 128 (Stored Uint8)
  ; tx_len  :: Stored (Ix 128)
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
  defStruct (Proxy :: Proxy "spi_transmission")
  defStruct (Proxy :: Proxy "spi_transaction_result")

