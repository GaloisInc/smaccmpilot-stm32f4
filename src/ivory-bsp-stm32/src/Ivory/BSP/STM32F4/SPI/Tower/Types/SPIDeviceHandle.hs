{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.BSP.STM32F4.SPI.Tower.Types.SPIDeviceHandle
  ( SPIDeviceHandle(..)
  ) where

import Ivory.Language

newtype SPIDeviceHandle = SPIDeviceHandle Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryStore, IvoryInit, IvoryZeroVal)

