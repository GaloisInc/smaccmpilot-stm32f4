{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{-

Commsec interface for sending/receiving MavLink messages.

Author: Lee Pike <leepike@galois.com>

-}

module SMACCMPilot.Flight.Commsec.Commsec
  ( encrypt
  , decrypt
  , uavCtx
  , copyToPkg
  , copyFromPkg
  , setupCommsec
  , commsecModule
  ) where

import Ivory.Language
import Ivory.Stdlib
import Data.Word

import qualified SMACCMPilot.Communications             as C
import qualified SMACCMPilot.Flight.Commsec.CommsecOpts as O

--------------------------------------------------------------------------------
-- Types and constants

headerLen :: Integer
headerLen = 8

-- Proxy type we'll cast from---doesn't really matter what the type (we don't
-- have void though).
type Commsec_ctx_proxy = Stored OpaqueType
type Key               = Array 16 (Stored Uint8)

--------------------------------------------------------------------------------
-- Import our API functions.

ivoryCommsec :: String
ivoryCommsec = "ivory-commsec.h"

commsec :: String
commsec = "commsec.h"

-- uint32_t securePkg_init( commsec_ctx *ctx, uint32_t myID
--                        , uint32_t decSalt, const uint8_t *rawDecKey
--                        , uint32_t encSalt, const uint8_t *rawEncKey);
securePkg_init ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Uint32
        , Uint32
        , ConstRef Global Key
        , Uint32
        , ConstRef Global Key
        ] :-> Uint32)
securePkg_init = importProc "securePkg_init" commsec

-- uint32_t securePkg_enc_in_place(
--   commsec_ctx *ctx, uint8_t *msg, uint32_t msgStartIdx, uint32_t msgLength
--                                );
securePkg_enc_in_place ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Ref s C.CommsecArray
        , Uint32
        , Uint32
        ] :-> Uint32)
securePkg_enc_in_place = importProc "securePkg_enc_in_place" commsec

-- uint32_t securePkg_dec(commsec_ctx *ctx, uint8_t *msg, uint32_t msgLen);
securePkg_dec ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Ref s C.CommsecArray
        , Uint32
        ] :-> Uint32)
securePkg_dec = importProc "securePkg_dec" commsec

--------------------------------------------------------------------------------

-- Contexts.  They're areas so thay can be shared by different tasks.
uavCtx :: MemArea Commsec_ctx_proxy
uavCtx  = importArea "uavCtx"  ivoryCommsec

--------------------------------------------------------------------------------

-- | Encrypt a package (with the header and tag) given a context.
encrypt :: MemArea Commsec_ctx_proxy
          -> Ref s C.CommsecArray
          -> Ivory eff ()
encrypt com pkg =
  call_ securePkg_enc_in_place
    (addrOf com) pkg (fromIntegral headerLen) (fromInteger C.mavlinkSize)

-- | Decrypt a package (with the header and tag), given a context.  Returns 0 if
-- all is OK and nonzero otherwise.  It is up to the caller to check the return
-- value.
decrypt :: MemArea Commsec_ctx_proxy
        -> Ref s C.CommsecArray
        -> Ivory eff Uint32
decrypt com pkg =
  call securePkg_dec (addrOf com) pkg (fromInteger C.commsecPkgSize)

--------------------------------------------------------------------------------

-- Copy a mavlink-sized message into our package buffer.
copyToPkg :: (GetAlloc eff ~ Scope s2)
         => ConstRef s0 C.MAVLinkArray
         -> Ref      s1 C.CommsecArray
         -> Ivory eff ()
copyToPkg from pkg = arrayCopy pkg from (fromInteger headerLen) (arrayLen from)

--------------------------------------------------------------------------------

-- Copy the payload out of a package buffer.
copyFromPkg :: (GetAlloc eff ~ Scope s2)
           => Ref s0 C.CommsecArray
           -> Ref s1 C.MAVLinkArray
           -> Ivory eff ()
copyFromPkg pkg from =
  arrayMap $ \(ix :: C.CommsecIx) ->
    when (ix >=? hdr .&& ix <? arrayLen from)
         $ do v <- deref (pkg ! ix)
              store (from ! mkIx ix) v
  where
  hdr = fromInteger headerLen
  mkIx = toIx . fromIx

--------------------------------------------------------------------------------

setupCommsec :: O.Options -> Ivory eff ()
setupCommsec opts =
  call_ securePkg_init (addrOf uavCtx)
                       (fromIntegral $ O.sendID opts)
                       (fromIntegral $ O.recvSalt opts)
                       (addrOf $ baseToUavKey $ O.recvKey opts)
                       (fromIntegral $ O.sendSalt opts)
                       (addrOf $ uavToBaseKey $ O.sendKey opts)

--------------------------------------------------------------------------------
-- Packaging

commsecModule :: O.Options -> Module
commsecModule opts = package "flight_commsec" $ do

  defConstMemArea (uavToBaseKey $ O.sendKey opts)
  defConstMemArea (baseToUavKey $ O.recvKey opts)
  defMemArea uavCtx

  inclHeader ivoryCommsec
  incl securePkg_init
  incl securePkg_enc_in_place

--------------------------------------------------------------------------------

mkKey :: [Word8] -> Init Key
mkKey key = iarray $ map (ival . fromIntegral) key

--------------------------------------------------------------------------------

uavToBaseKey, baseToUavKey :: [Word8] -> ConstMemArea Key
uavToBaseKey = constArea "uav_to_base_key" . mkKey
baseToUavKey = constArea "base_to_uav_key" . mkKey

--------------------------------------------------------------------------------
