{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Types.ArmedMode
  ( ArmedMode()
  , safe
  , disarmed
  , armed
  ) where

import Ivory.Language

newtype ArmedMode = ArmedMode Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

safe :: ArmedMode
safe = ArmedMode 0

disarmed :: ArmedMode
disarmed = ArmedMode 1

armed :: ArmedMode
armed = ArmedMode 2

