{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4IO.Types.RequestCode
  ( RequestCode(..)
  , request_read
  , request_write
  , request_ok
  , request_corrupt
  , request_error
  ) where

import Ivory.Language
import Ivory.Serialize

newtype RequestCode = RequestCode { unRequestCode :: Uint8 }
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance Packable ('Stored RequestCode) where
  packRep = repackV wrap unwrap packRep
    where
    wrap raw = RequestCode $ (raw <? 5) ? (raw, 4)
    unwrap (RequestCode src) = src

request_read    :: RequestCode
request_read     = RequestCode 0
request_write   :: RequestCode
request_write    = RequestCode 1
request_ok      :: RequestCode
request_ok       = RequestCode 2
request_corrupt :: RequestCode
request_corrupt  = RequestCode 3
request_error   :: RequestCode
request_error    = RequestCode 4

