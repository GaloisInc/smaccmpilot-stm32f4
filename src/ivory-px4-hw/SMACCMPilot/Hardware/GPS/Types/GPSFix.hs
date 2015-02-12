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
import Ivory.Serialize

newtype GPSFix = GPSFix Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance Packable (Stored GPSFix) where
  packRep = repackV wrap unwrap packRep
    where
    wrap raw = GPSFix $ (raw ==? 2 .|| raw ==? 3) ? (raw, 0)
    unwrap (GPSFix src) = src

fix_none, fix_2d, fix_3d :: GPSFix
fix_none = GPSFix 0
fix_2d   = GPSFix 2
fix_3d   = GPSFix 3

