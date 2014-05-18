{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.YawMode
  ( YawMode()
  , rate
  , heading
  ) where

import Ivory.Language

newtype YawMode = YawMode Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

instance IvorySizeOf (Stored YawMode) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

rate :: YawMode
rate = YawMode 0

heading :: YawMode
heading = YawMode 1

