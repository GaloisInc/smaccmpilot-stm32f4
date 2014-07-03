{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.I2C.Types where

import Ivory.Language
import Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr

[ivory|
struct i2c_transaction_request
  { tx_addr   :: Stored I2CDeviceAddr
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  ; rx_len    :: Stored (Ix 128)
  }

struct i2c_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  }

|]
