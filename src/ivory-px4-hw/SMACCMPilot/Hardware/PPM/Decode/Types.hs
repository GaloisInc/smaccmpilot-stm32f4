{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM.Decode.Types
  ( PPMState()
  , ppmSync
  , ppmMark
  , ppmSpace
  ) where

import Ivory.Language

newtype PPMState = PPMState Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

ppmSync :: PPMState
ppmSync  = PPMState 0

ppmMark :: PPMState
ppmMark = PPMState 1

ppmSpace :: PPMState
ppmSpace = PPMState 2

