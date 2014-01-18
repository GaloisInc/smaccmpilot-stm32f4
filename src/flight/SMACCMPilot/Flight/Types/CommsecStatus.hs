{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.CommsecStatus
  ( CommsecStatus()
  , alarm
  , secure
  ) where

import Ivory.Language

newtype CommsecStatus = CommsecStatus Uint32
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

alarm :: CommsecStatus
alarm  = CommsecStatus 0

secure :: CommsecStatus
secure  = CommsecStatus 1

