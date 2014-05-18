{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.UISource
  ( UISource()
  , ppm
  , mavlink
  ) where

import Ivory.Language

newtype UISource = UISource Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal )

instance IvorySizeOf (Stored UISource) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

ppm :: UISource
ppm = UISource 0

mavlink :: UISource
mavlink = UISource 1

