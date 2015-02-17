{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM.Decode.Types
  ( PPMState()
  , ppmSync
  , ppmArming
  , ppmActive
  , ppmInactive
  ) where

import Ivory.Language

newtype PPMState = PPMState Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

ppmSync :: PPMState
ppmSync  = PPMState 0

ppmArming :: PPMState
ppmArming  = PPMState 1

ppmActive :: PPMState
ppmActive  = PPMState 2

ppmInactive :: PPMState
ppmInactive  = PPMState 3
