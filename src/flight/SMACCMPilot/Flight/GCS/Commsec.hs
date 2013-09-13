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

module SMACCMPilot.Flight.GCS.Commsec
  -- ( initializePackage
  -- , encrypt
  -- , decrypt
  -- ) 
    where

import Ivory.Language
import Ivory.Stdlib

--------------------------------------------------------------------------------
-- Types and constants

-- Encrypt 128 byte chunks minus room for the header and tag (8 bytes each).
type PkgArr            = Array 112 (Stored Uint8)
type Pkg s             = Ref s PkgArr
type PkgIx             = Ix 112

-- Replicates macros TAG_LEN and HEADER_LEN
-- Must match types given above.
maxMsgLen, tagLen, headerLen :: Int
maxMsgLen = 112
tagLen    = 8
headerLen = 8

-- Proxy type we'll cast from---doesn't really matter what the type (we don't
-- have void though).
type Commsec_ctx_proxy = Stored OpaqueType
type Key               = Array 16 (Stored Uint8)

packageSize :: Uint32
packageSize = arrayLen (undefined :: Pkg s)

mkIx :: Int -> PkgIx
mkIx c = toIx (fromIntegral c :: Uint32)

maxMsgLenI, tagLenI, headerLenI :: PkgIx
maxMsgLenI = mkIx maxMsgLen
tagLenI    = mkIx tagLen
headerLenI = mkIx headerLen

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
        , Pkg s
        , Uint32
        , Uint32
        ] :-> Uint32)
securePkg_enc_in_place = importProc "securePkg_enc_in_place" commsec

-- uint32_t securePkg_dec(commsec_ctx *ctx, uint8_t *msg, uint32_t msgLen);
securePkg_dec ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Pkg s
        , Uint32
        ] :-> Uint32)
securePkg_dec = importProc "securePkg_dec" commsec

--------------------------------------------------------------------------------

-- Contexts.
uav, base :: MemArea Commsec_ctx_proxy
uav  = importArea "uav"  ivoryCommsec
base = importArea "base" ivoryCommsec

uavID :: Uint32
uavID  = 0

mkKey :: [Int] -> Init Key
mkKey key = iarray $ map (ival . fromIntegral) key

uavToBaseKey, baseToUavKey :: ConstMemArea Key
uavToBaseKey = constArea "uav_to_base_key" (mkKey [0..15])
baseToUavKey = constArea "base_to_uav_key" (mkKey [15,14..0])

b2uSalt, u2bSalt :: Uint32
b2uSalt = 9219834
u2bSalt = 284920

-- someMsg :: String
-- someMsg = "This is a message from "

-- mkMsg :: String -> String
-- mkMsg msg = m ++ replicate n ' '
--   where m = someMsg ++ msg
--         n = maxMsgLen - length m

--------------------------------------------------------------------------------

-- initializePackage :: ConstRef s (Array 112 (Stored Uint8)) -> Init PkgArr
-- initializePackage arr = undefined -- do
  -- arr <- local (iarray 
  -- iarray $
  --    replicate headerLen izero
  -- ++ map ival arr
  -- ++ replicate tagLen izero

encrypt :: MemArea Commsec_ctx_proxy
          -> Pkg s
          -> Ivory eff ()
encrypt com pkg =
  call_ securePkg_enc_in_place
    (addrOf com) pkg (fromIntegral headerLen) (fromIntegral maxMsgLen)

decrypt :: MemArea Commsec_ctx_proxy
        -> Pkg s
        -> Ivory eff ()
decrypt com pkg = call_ securePkg_dec (addrOf com) pkg packageSize

--------------------------------------------------------------------------------
-- Packaging and testing.

commsecModule :: Module
commsecModule = package "IvoryGCM" $ do

  defConstMemArea uavToBaseKey
  defConstMemArea baseToUavKey

  inclHeader ivoryCommsec
  inclHeader ivoryCommsec
  incl securePkg_init
  incl securePkg_enc_in_place
  defMemArea uavPkg

--------------------------------------------------------------------------------

setupCommsec :: Ivory eff ()
setupCommsec =
  call_ securePkg_init (addrOf uav) uavID
                       b2uSalt (addrOf baseToUavKey)
                       u2bSalt (addrOf uavToBaseKey)

uavPkg :: MemArea (Array 128 (Stored Uint8))
uavPkg = area "uavPkg" Nothing

-- Copy a messge from an arbitrary-sized array into our package buffer.  If the
-- message array is too small, the buffer is padded with zeros.  If it's too
-- large, no copying is done.  Returns True if the copy was successful and False
-- if no copying is done.
cpyToPkg :: (SingI n)
         => ConstRef s (Array n (Stored Uint8))
         -> Ivory eff ()
cpyToPkg from =
  if fromTooBig
    then err
    else
      arrayMap $ \(ix :: Ix 128) ->
        cond_ [   ix <? headerEndIx
              ==> return ()
              ,   ix >=? payloadEndIx
              ==> return ()
              ,   fromEnd ix -- from array too short: fill with zeros.
              ==> store (pkg ! ix) 0
              ,   true
              ==> deref (from ! toIx (fromIx ix)) >>= store (pkg ! ix)
              ]
  where
  pkg        = addrOf uavPkg
  lenFrom    = arrayLen from
  lenTo      = arrayLen pkg
  fromEnd    :: Ix 128 -> IBool
  fromEnd ix = ix >=? fromIntegral lenFrom
  fromTooBig :: Bool
  fromTooBig = lenTo - tagLen - headerLen < lenFrom

  headerEndIx  = fromIntegral headerLen
  payloadEndIx = lenTo - fromIntegral tagLen

  err = error $
    "cpyToPkg in Flight/GCS/Commsec.hs: mavlink array too big.  " ++
    "from array length: " ++ show lenFrom
