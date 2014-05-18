{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.ThrottleMode
  ( ThrottleMode()
  , direct
  , autothrottle
  ) where

import Ivory.Language

newtype ThrottleMode = ThrottleMode Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

instance IvorySizeOf (Stored ThrottleMode) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

direct :: ThrottleMode
direct = ThrottleMode 0

autothrottle :: ThrottleMode
autothrottle = ThrottleMode 1

