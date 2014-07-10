{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32.Driver.I2C.I2CDriverState
  ( I2CDriverState
  , stateInactive
  , stateActive
  , stateError
  ) where

import Ivory.Language

newtype I2CDriverState = I2CDriverState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

stateInactive :: I2CDriverState
stateInactive =  I2CDriverState 0
stateActive   :: I2CDriverState
stateActive   =  I2CDriverState 1
stateError    :: I2CDriverState
stateError    =  I2CDriverState 2

