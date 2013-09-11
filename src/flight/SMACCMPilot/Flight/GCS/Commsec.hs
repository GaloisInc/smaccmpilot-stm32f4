{-# LANGUAGE TypeOperators #-}
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
  ( initializePackage
  , encrypt
  , decrypt
  ) where

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

uavID, baseID :: Uint32
uavID  = 0
baseID = 0

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

initializePackage :: ConstRef s (Array 112 (Stored Uint8)) -> Init PkgArr
initializePackage arr = do
  arr <- local (iarray 
  iarray $
     replicate headerLen izero
  ++ map ival arr
  ++ replicate tagLen izero

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

pkg :: Module
pkg = package "IvoryGCM" $ do

  defConstMemArea uavToBaseKey
  defConstMemArea baseToUavKey

  inclHeader ivoryCommsec
  incl securePkg_init
  incl securePkg_enc_in_place

--------------------------------------------------------------------------------

-- test :: Def('[] :-> ())
-- test = proc "test" $ body $ do
--   -- Allocate memory on the stack for the package (extra room for head/tail)
--   packageFromUAV   <- local $ initializePackage (mkMsg "uav!")
--   packageFromBase0 <- local $ initializePackage (mkMsg "base0!")
--   packageFromBase1 <- local $ initializePackage (mkMsg "base1!")

--   call_ securePkg_init
--     (addrOf uav)   uavID
--     b2uSalt (addrOf baseToUavKey)
--     u2bSalt (addrOf uavToBaseKey)
--   call_ securePkg_init
--     (addrOf base0) base0ID
--     u2bSalt (addrOf uavToBaseKey)
--     b2uSalt (addrOf baseToUavKey)
--   call_ securePkg_init
--     (addrOf base1) base1ID
--     u2bSalt (addrOf uavToBaseKey)
--     b2uSalt (addrOf baseToUavKey)

--   printMsgs "%c" packageFromUAV packageFromBase0

--   encrypt uav   packageFromUAV
--   encrypt base0 packageFromBase0
--   encrypt base1 packageFromBase1

--   printMsgs "%02x" packageFromUAV packageFromBase0

--   decrypt uav   packageFromBase0
-- --  decrypt uav   packageFromBase1
--   decrypt base0 packageFromUAV

--   printMsgs "%c" packageFromUAV packageFromBase0

--   retVoid

-- printMsgs :: IString
--           -> Pkg s
--           -> Pkg s
--           -> Ivory eff ()
-- printMsgs fmt packageFromUAV packageFromBase0 = do
--   call_ printf "From UAV: "
--   call_ printMsg fmt  packageFromUAV
--   call_ printf "From Base0: "
--   call_ printMsg fmt packageFromBase0

-- main :: IO ()
-- main = runCompiler [pkg] initialOpts { stdOut = True }

-- build :: IO ()
-- build = runCompiler [pkg] initialOpts { stdOut     = False
--                                       , constFold  = True
--                                       , srcDir     = "ivory-gen"
--                                       , includeDir = "ivory-gen"
--                                       }
