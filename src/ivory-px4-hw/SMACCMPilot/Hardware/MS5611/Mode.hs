{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.MS5611.Mode
  ( ContinuationMode()
  , idle
  , sendReset
  , waitReset
  , initializing
  , running
  ) where

import Ivory.Language

newtype ContinuationMode = ContinuationMode Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

idle :: ContinuationMode
idle = ContinuationMode 0

sendReset :: ContinuationMode
sendReset = ContinuationMode 1

waitReset :: ContinuationMode
waitReset = ContinuationMode 2

initializing :: ContinuationMode
initializing = ContinuationMode 3

running :: ContinuationMode
running = ContinuationMode 4

