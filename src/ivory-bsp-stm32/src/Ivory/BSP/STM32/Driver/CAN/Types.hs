{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.CAN.Types where

import Ivory.Language

[ivory|
struct can_transmit_request
  { tx_id     :: Stored Uint32
  ; tx_ide    :: Stored IBool
  ; tx_rtr    :: Stored IBool
  ; tx_buf    :: Array 8 (Stored Uint8)
  ; tx_len    :: Stored (Ix 9)
  }

struct can_receive_result
  { rx_id     :: Stored Uint32
  ; rx_ide    :: Stored IBool
  ; rx_rtr    :: Stored IBool
  ; rx_buf    :: Array 8 (Stored Uint8)
  ; rx_len    :: Stored (Ix 9)
  ; rx_fmi    :: Stored Uint8
  ; rx_time   :: Stored Uint16
  }
|]

canDriverTypes :: Module
canDriverTypes = package "canDriverTypes" $ do
  defStruct (Proxy :: Proxy "can_transmit_request")
  defStruct (Proxy :: Proxy "can_receive_result")
