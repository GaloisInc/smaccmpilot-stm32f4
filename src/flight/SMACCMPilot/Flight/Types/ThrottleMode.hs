{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.ThrottleMode
  ( ThrottleMode()
  , direct
  , autothrottle
  ) where

import Ivory.Language

newtype ThrottleMode = ThrottleMode Uint32
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

direct :: ThrottleMode
direct = ThrottleMode 0

autothrottle :: ThrottleMode
autothrottle = ThrottleMode 1

