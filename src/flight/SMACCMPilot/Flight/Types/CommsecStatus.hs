{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.CommsecStatus
  ( CommsecStatus()
  , alarm
  , secure
  ) where

import Ivory.Language

newtype CommsecStatus = CommsecStatus Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

instance IvorySizeOf (Stored CommsecStatus) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

alarm :: CommsecStatus
alarm  = CommsecStatus 0

secure :: CommsecStatus
secure  = CommsecStatus 1

