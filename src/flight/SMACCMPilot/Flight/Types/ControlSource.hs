{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.ControlSource
  ( ControlSource()
  , ui
  , nav
  ) where

import Ivory.Language

newtype ControlSource = ControlSource Uint32
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

ui :: ControlSource
ui = ControlSource 0

nav :: ControlSource
nav = ControlSource 1

