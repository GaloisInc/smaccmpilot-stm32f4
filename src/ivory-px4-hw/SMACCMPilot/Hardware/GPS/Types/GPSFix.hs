{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.GPS.Types.GPSFix
  ( GPSFix
  , fix_none
  , fix_2d
  , fix_3d
  ) where

import Ivory.Language

newtype GPSFix = GPSFix Uint32
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance IvorySizeOf (Stored GPSFix) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

fix_none, fix_2d, fix_3d :: GPSFix
fix_none = GPSFix 0
fix_2d   = GPSFix 1
fix_3d   = GPSFix 2

