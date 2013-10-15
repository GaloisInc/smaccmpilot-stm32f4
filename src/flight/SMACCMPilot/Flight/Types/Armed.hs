{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Valid arming modes.

module SMACCMPilot.Flight.Types.Armed
  ( ArmedMode ()
  , as_DISARMED
  , as_ARMED
  , fromArmedMode
  , armedModes
  ) where

import Ivory.Language

newtype ArmedMode = Armed Uint8
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

as_DISARMED, as_ARMED :: ArmedMode
as_DISARMED = Armed 0
as_ARMED    = Armed 1

fromArmedMode :: ArmedMode -> Uint8
fromArmedMode (Armed n) = n

armedModes :: [ArmedMode]
armedModes = [ as_DISARMED, as_ARMED ]
