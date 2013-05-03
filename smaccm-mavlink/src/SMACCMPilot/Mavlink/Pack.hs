{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Mavlink.Pack
  ( MavlinkPackable
  , pack, unpack, packedSize
  , packModule
  , arrayPack, arrayUnpack
  ) where

import Ivory.Language

packModule :: Module
packModule = package "mavlink_pack_ivory" $ do
  inclHeader "flight-support/pack.h" --  in flight support

class (IvoryExpr t) => MavlinkPackable t where
  pack       :: Def ('[Ref s (CArray (Stored Uint8)), Uint8, t]   :-> ())
  unpack     :: Def ('[ConstRef s (CArray (Stored Uint8)), Uint8] :-> t)
  packedSize :: t -> Int

instance MavlinkPackable Uint64 where
 pack         = importProc "mavlink_pack_uint64_t"   "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_uint64_t" "flight-support/pack.h"
 packedSize _ = 8
instance MavlinkPackable Sint64 where
 pack         = importProc "mavlink_pack_int64_t"    "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_int64_t"  "flight-support/pack.h"
 packedSize _ = 8
instance MavlinkPackable Uint32 where
 pack         = importProc "mavlink_pack_uint32_t"   "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_uint32_t" "flight-support/pack.h"
 packedSize _ = 4
instance MavlinkPackable Sint32 where
 pack         = importProc "mavlink_pack_int32_t"    "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_int32_t"  "flight-support/pack.h"
 packedSize _ = 4
instance MavlinkPackable Uint16 where
 pack         = importProc "mavlink_pack_uint16_t"   "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_uint16_t" "flight-support/pack.h"
 packedSize _ = 2
instance MavlinkPackable Sint16 where
 pack         = importProc "mavlink_pack_int16_t"    "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_int16_t"  "flight-support/pack.h"
 packedSize _ = 2
instance MavlinkPackable Uint8 where
 pack         = importProc "mavlink_pack_uint8_t"    "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_uint8_t"  "flight-support/pack.h"
 packedSize _ = 1
-- Treat "char" as a "uint8_t" for packing purposes.
instance MavlinkPackable IChar where
 pack         = importProc "mavlink_pack_uint8_t"    "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_uint8_t"  "flight-support/pack.h"
 packedSize _ = 1
instance MavlinkPackable Sint8 where
 pack         = importProc "mavlink_pack_int8_t"     "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_int8_t"   "flight-support/pack.h"
 packedSize _ = 1
instance MavlinkPackable IFloat where
 pack         = importProc "mavlink_pack_float"      "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_float"    "flight-support/pack.h"
 packedSize _ = 4
instance MavlinkPackable IDouble where
 pack         = importProc "mavlink_pack_double"     "flight-support/pack.h"
 unpack       = importProc "mavlink_unpack_double"   "flight-support/pack.h"
 packedSize _ = 8

arrayPack :: (SingI len, MavlinkPackable rep)
           => Ref s1 (CArray (Stored Uint8))
           -> Uint8
           -> ConstRef s2 (Array len (Stored rep))
           -> Ivory eff ()
arrayPack dst offs src = do
  arr <- assign src  -- Give the source array a local name
  arrayMap $ \ix -> do -- Produce a loop of pack calls
    call_ pack dst (offs + safeCast ix) =<< deref (arr ! ix)

arrayUnpack :: (SingI len, MavlinkPackable rep, IvoryStore rep)
            => ConstRef s1 (CArray (Stored Uint8))
            -> Uint8
            -> Ref s (Array len (Stored rep))
            -> Ivory eff ()
arrayUnpack src offs dest = do
  arrayMap $ \ix -> do
    val <- call unpack src (offs + safeCast ix)
    store (dest ! ix) val
