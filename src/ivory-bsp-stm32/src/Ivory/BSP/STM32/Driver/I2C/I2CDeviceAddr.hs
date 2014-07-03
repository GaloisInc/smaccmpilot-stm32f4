{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr
  ( I2CDeviceAddr(..)
  , readAddr
  , writeAddr
  ) where

import Ivory.Language

newtype I2CDeviceAddr = I2CDeviceAddr Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryStore, IvoryInit, IvoryZeroVal)

instance IvorySizeOf (Stored I2CDeviceAddr) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint8))

readAddr :: I2CDeviceAddr -> Uint8
readAddr (I2CDeviceAddr a) = 2 * a + 1
writeAddr :: I2CDeviceAddr -> Uint8
writeAddr (I2CDeviceAddr a) = 2 * a

