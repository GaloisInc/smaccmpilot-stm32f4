{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4IO.Types.RequestCode
  ( RequestCode
  , request_read
  , request_write
  , request_corrupt
  , request_error
  ) where

import Ivory.Language
import Ivory.Serialize

newtype RequestCode = RequestCode Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance Packable (Stored RequestCode) where
  packRep = repackV wrap unwrap packRep
    where
    wrap raw = RequestCode $ (raw <? 4) ? (raw, 0)
    unwrap (RequestCode src) = src

request_read    :: RequestCode
request_read     = RequestCode 0
request_write   :: RequestCode
request_write    = RequestCode 1
request_corrupt :: RequestCode
request_corrupt  = RequestCode 2
request_error   :: RequestCode
request_error    = RequestCode 3

