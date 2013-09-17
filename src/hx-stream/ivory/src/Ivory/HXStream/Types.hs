{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.HXStream.Types
  ( HXState
  , hxstate_idle
  , hxstate_fstart
  , hxstate_data
  , hxstate_esc
  ) where

import Ivory.Language

-- Idea: this pattern is useful, could we make an ivoryenum quasiquoter
-- for defining these automatically?

newtype HXState =
  HXState
    { unHXState :: Uint8
    } deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

hxstate_idle, hxstate_fstart, hxstate_data, hxstate_esc :: HXState
hxstate_idle   = HXState 0
hxstate_fstart = HXState 1
hxstate_data   = HXState 2
hxstate_esc    = HXState 3
