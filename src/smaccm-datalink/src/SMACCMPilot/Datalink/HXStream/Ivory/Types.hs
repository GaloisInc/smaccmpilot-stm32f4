{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Datalink.HXStream.Ivory.Types
  ( HXState
  , hxstate_tag
  , hxstate_progress
  , hxstate_esc
  ) where

import Ivory.Language

-- Idea: this pattern is useful, could we make an ivoryenum quasiquoter
-- for defining these automatically?

newtype HXState = HXState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

hxstate_tag, hxstate_progress, hxstate_esc  :: HXState
hxstate_tag        = HXState 1
hxstate_progress   = HXState 2
hxstate_esc        = HXState 3
