{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Commsec.Ivory.Error
  ( GecError
  , success
  , fail_bad_input
  , fail_dup_ctr
  , fail_gcm_auth
  , fail_ctr_rollover
  , fail_encrypt_gcm
  ) where

import Ivory.Language
import Ivory.Serialize

newtype GecError = GecError Uint32
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance Packable (Stored GecError) where
  packRep = repackV wrap unwrap packRep
    where
    wrap raw = GecError $ (raw <=? 6) ? (raw, 0)
    unwrap (GecError src) = src

success, fail_bad_input, fail_dup_ctr, fail_ctr_rollover, fail_gcm_auth, fail_encrypt_gcm :: GecError
success           = GecError 0
fail_bad_input    = GecError 1
fail_dup_ctr      = GecError 2
fail_gcm_auth     = GecError 3
fail_ctr_rollover = GecError 4
fail_encrypt_gcm  = GecError 5
