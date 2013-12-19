{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.YawMode
  ( YawMode()
  , rate
  , heading
  ) where

import Ivory.Language

newtype YawMode = YawMode Uint32
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

rate :: YawMode
rate = YawMode 0

heading :: YawMode
heading = YawMode 1

