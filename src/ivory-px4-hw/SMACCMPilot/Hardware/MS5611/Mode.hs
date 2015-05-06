{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.MS5611.Mode
  ( ContinuationMode()
  , idle
  , sendReset
  , resetSent
  , waitReset
  , waitSent
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

resetSent :: ContinuationMode
resetSent = ContinuationMode 2

waitReset :: ContinuationMode
waitReset = ContinuationMode 3

waitSent :: ContinuationMode
waitSent = ContinuationMode 4

initializing :: ContinuationMode
initializing = ContinuationMode 5

running :: ContinuationMode
running = ContinuationMode 6


