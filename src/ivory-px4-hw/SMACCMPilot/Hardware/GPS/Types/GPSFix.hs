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

instance IvorySizeOf (Stored GPSFix) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint8))

instance SerializableRef (Stored GPSFix)
instance Serializable GPSFix where
  pack dst offs (GPSFix src) = pack dst offs src
  unpack src offs = do
    raw <- unpack src offs
    return $ GPSFix $ (raw ==? 2 .|| raw ==? 3) ? (raw, 0)

fix_none, fix_2d, fix_3d :: GPSFix
fix_none = GPSFix 0
fix_2d   = GPSFix 2
fix_3d   = GPSFix 3

