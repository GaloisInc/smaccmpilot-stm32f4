{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32.Driver.SPI.SPIDeviceHandle
  ( SPIDeviceHandle(..)
  ) where

import Ivory.Language

newtype SPIDeviceHandle = SPIDeviceHandle Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryStore, IvoryInit, IvoryZeroVal)

instance IvorySizeOf (Stored SPIDeviceHandle) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint8))

