{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.EnableDisable
  ( EnableDisable()
  , none
  , enable
  , disable
  ) where

import Ivory.Language

newtype EnableDisable = EnableDisable Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal )

instance IvorySizeOf (Stored EnableDisable) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint8))

none :: EnableDisable
none = EnableDisable 0

enable :: EnableDisable
enable = EnableDisable 1

disable :: EnableDisable
disable = EnableDisable 2
