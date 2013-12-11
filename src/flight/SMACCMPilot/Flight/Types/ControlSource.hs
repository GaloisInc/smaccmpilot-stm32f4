{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.Types.ControlSource
  ( ControlSource()
  , ppm
  , mavlink
  , auto
  ) where

import Ivory.Language

newtype ControlSource = ControlSource Uint32
  deriving (Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

ppm :: ControlSource
ppm = ControlSource 0

mavlink :: ControlSource
mavlink = ControlSource 1

auto :: ControlSource
auto = ControlSource 2

