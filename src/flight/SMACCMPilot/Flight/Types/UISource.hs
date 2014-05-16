{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.UISource
  ( UISource()
  , ppm
  , mavlink
  ) where

import Ivory.Language

newtype UISource = UISource Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal )

ppm :: UISource
ppm = UISource 0

mavlink :: UISource
mavlink = UISource 1

