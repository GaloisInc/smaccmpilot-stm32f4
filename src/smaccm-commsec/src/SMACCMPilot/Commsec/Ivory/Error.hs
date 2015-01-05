{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Commsec.Ivory.Error
  ( CommsecError
  , success
  , fail_bad_base_station
  , fail_dup_ctr
  , fail_ctr_rollover
  , fail_bad_input
  , fail_msg_length
  , fail_gcm
  ) where

import Ivory.Language
import Ivory.Serialize

newtype CommsecError = CommsecError Uint32
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

instance IvorySizeOf (Stored CommsecError) where
  sizeOfBytes _ = sizeOfBytes (Proxy :: Proxy (Stored Uint32))

instance SerializableRef (Stored CommsecError)
instance Serializable CommsecError where
  pack dst offs (CommsecError src) = pack dst offs src
  unpack src offs = do
    raw <- unpack src offs
    return $ CommsecError $ (raw <=? 6) ? (raw, 0)

success :: CommsecError
success = CommsecError 0
fail_bad_base_station :: CommsecError
fail_bad_base_station = CommsecError 1
fail_dup_ctr :: CommsecError
fail_dup_ctr = CommsecError 2
fail_ctr_rollover :: CommsecError
fail_ctr_rollover = CommsecError 3
fail_bad_input :: CommsecError
fail_bad_input = CommsecError 4
fail_msg_length :: CommsecError
fail_msg_length = CommsecError 5
fail_gcm :: CommsecError
fail_gcm = CommsecError 6

