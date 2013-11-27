{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.UserInputSource
  ( UISource()
  , uiSourcePPM
  , uiSourceRCOverride
  ) where

import Ivory.Language

newtype UISource = UISource Uint8
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

uiSourcePPM, uiSourceRCOverride :: UISource
uiSourcePPM        = UISource 0
uiSourceRCOverride = UISource 1

