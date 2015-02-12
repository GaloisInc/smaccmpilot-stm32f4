{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.GPS.UBlox.Types
  ( UBXState
  , ubx_idle
  , ubx_sync
  , ubx_class
  , ubx_id
  , ubx_len1
  , ubx_len2
  , ubx_payload
  , ubx_cksum
  ) where

import Ivory.Language

newtype UBXState = UBXState Uint32
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

ubx_idle    :: UBXState
ubx_idle     = UBXState 0
ubx_sync    :: UBXState
ubx_sync     = UBXState 1
ubx_class   :: UBXState
ubx_class    = UBXState 2
ubx_id      :: UBXState
ubx_id       = UBXState 3
ubx_len1    :: UBXState
ubx_len1     = UBXState 4
ubx_len2    :: UBXState
ubx_len2     = UBXState 5
ubx_payload :: UBXState
ubx_payload  = UBXState 6
ubx_cksum   :: UBXState
ubx_cksum    = UBXState 7

