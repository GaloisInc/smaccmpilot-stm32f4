{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-

Interface for AES GCM.

Author: Lee Pike <leepike@galois.com>

-}

module Main where

--------------------------------------------------------------------------------

import Ivory.Language
-- XXX testing
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Stdlib

--------------------------------------------------------------------------------
-- Constants

type PkgArr = Array 100 (Stored Uint8)
type Pkg s = Ref s PkgArr

packageSize :: Uint32
packageSize = arrayLen (undefined :: Pkg s)

type PkgIx = Ix 100

-- Replicates macros TAG_LEN and HEADER_LEN
-- Cannot be bigger than the 100
maxMsgLen, tagLen, headerLen :: Int
maxMsgLen = 84
tagLen    = 8
headerLen = 8

mkIx :: Int -> PkgIx
mkIx c = toIx (fromIntegral c :: Uint32)

maxMsgLenI, tagLenI, headerLenI :: PkgIx
maxMsgLenI = mkIx maxMsgLen
tagLenI    = mkIx tagLen
headerLenI = mkIx headerLen

--------------------------------------------------------------------------------
-- General purpose imports

printf :: Def('[IString] :-> ())
printf = importProc "printf" "stdio.h"

printf8 :: Def('[IString, Uint8] :-> ())
printf8 = importProc "printf" "stdio.h"

printf32 :: Def('[IString, Uint32] :-> ())
printf32 = importProc "printf" "stdio.h"

printMsg :: Def('[IString, Pkg s] :-> ())
printMsg = proc "printArr" $ \str arr -> body $ do
  call_ printf "msg: "
  arrayMap $ \ix -> do
    val <- deref (arr ! ix)
    cond_ [ ix <? headerLenI               ==> return ()
          , ix >=? headerLenI + maxMsgLenI ==> return ()
--          , val ==? 32                     ==> return () -- space
          , true                           ==> call_ printf8 str val
          ]
  call_ printf "\n"

--------------------------------------------------------------------------------
-- Import our API functions.

ivoryShim :: String
ivoryShim = "ivory-commsec-shim.h"

commsec :: String
commsec = "aeslib/commsec.h"

type Commsec_ctx_proxy = Stored OpaqueType

type Key = Array 16 (Stored Uint8)

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

securePkg_enc_in_place ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Pkg s
        , Uint32
        , Uint32
        ] :-> Uint32)
securePkg_enc_in_place = importProc "securePkg_enc_in_place" commsec

securePkg_dec ::
  Def ('[ Ref Global Commsec_ctx_proxy
        , Pkg s
        , Uint32
        ] :-> Uint32)
securePkg_dec = importProc "securePkg_dec" commsec

--------------------------------------------------------------------------------

uav, base0, base1 :: MemArea Commsec_ctx_proxy
uav   = importArea "uav"   ivoryShim
base0 = importArea "base0" ivoryShim
base1 = importArea "base1" ivoryShim

uavID, base0ID, base1ID :: Uint32
uavID   = 0
base0ID = 0
base1ID = 1

mkKey :: [Int] -> Init Key
mkKey key = iarray $ map (ival . fromIntegral) key

uavToBaseKey, baseToUavKey :: ConstMemArea Key
uavToBaseKey = constArea "uav_to_base_key" (mkKey [0..15])
baseToUavKey = constArea "base_to_uav_key" (mkKey [15,14..0])

b2uSalt, u2bSalt :: Uint32
b2uSalt = 9219834
u2bSalt = 284920

someMsg :: String
someMsg = "This is a message from "

mkMsg :: String -> String
mkMsg msg = m ++ replicate n ' '
  where m = someMsg ++ msg
        n = maxMsgLen - length m

--------------------------------------------------------------------------------

initializePackage :: String -> Init PkgArr
initializePackage msg = iarray $
     replicate headerLen izero
  ++ map (ival . fromIntegral . fromEnum) msg
  ++ replicate tagLen izero

test :: Def('[] :-> ())
test = proc "test" $ body $ do
  -- Allocate memory on the stack for the package (extra room for head/tail)
  -- packages are arrays we'll encrypt in-place.
  packageFromUAV   <- local $ initializePackage (mkMsg "uav!")
  packageFromBase0 <- local $ initializePackage (mkMsg "base0!")
  packageFromBase1 <- local $ initializePackage (mkMsg "base1!")

  call_ securePkg_init
    (addrOf uav)   uavID
    b2uSalt (addrOf baseToUavKey)
    u2bSalt (addrOf uavToBaseKey)
  call_ securePkg_init
    (addrOf base0) base0ID
    u2bSalt (addrOf uavToBaseKey)
    b2uSalt (addrOf baseToUavKey)
  call_ securePkg_init
    (addrOf base1) base1ID
    u2bSalt (addrOf uavToBaseKey)
    b2uSalt (addrOf baseToUavKey)

  printMsgs "%c" packageFromUAV packageFromBase0

  -- uav, base0, etc. are pointers to structs containing crypto-gsm info.
  _ <- encrypt uav   packageFromUAV
  _ <- encrypt base0 packageFromBase0
  _ <- encrypt base1 packageFromBase1

  printMsgs "%02x" packageFromUAV packageFromBase0

  -- decrypt and check the authentication of the encrypted packageFromBase0
C  -- using uav's crypto info.  This represents the UAV getting a message from
  -- base0.
  decAndCheckRes uav packageFromBase0

  decAndCheckRes base0 packageFromUAV

  printMsgs "%c" packageFromUAV packageFromBase0

  -- Decrypting twice!
  decAndCheckRes base0 packageFromUAV

  printMsgs "%c" packageFromUAV packageFromBase0

  retVoid

decAndCheckRes :: MemArea Commsec_ctx_proxy -> Pkg s -> Ivory eff ()
decAndCheckRes ctx pkg = do
  res <- decrypt ctx pkg
  (call_ printf32 " res (should be 0): %u\n" res)
  -- when (res /=? 0) (call_ printf32 "  error in decryption! %u\n" res)

encrypt :: MemArea Commsec_ctx_proxy
          -> Pkg s
          -> Ivory eff Uint32
encrypt com pkg =
  call securePkg_enc_in_place
    (addrOf com) pkg (fromIntegral headerLen) (fromIntegral maxMsgLen)

decrypt :: MemArea Commsec_ctx_proxy
        -> Pkg s
        -> Ivory eff Uint32
decrypt com pkg = call securePkg_dec (addrOf com) pkg packageSize

printMsgs :: IString
          -> Pkg s
          -> Pkg s
          -> Ivory eff ()
printMsgs fmt packageFromUAV packageFromBase0 = do
  call_ printf "From UAV: "
  call_ printMsg fmt  packageFromUAV
  call_ printf "From Base0: "
  call_ printMsg fmt packageFromBase0

--------------------------------------------------------------------------------
-- Packaging and testing.

modulePkg :: Module
modulePkg = package "IvoryGCM" $ do

  defConstMemArea uavToBaseKey
  defConstMemArea baseToUavKey
--  defStruct (Proxy :: Proxy "commsec_ctx")

  inclHeader ivoryShim
  incl securePkg_init
  incl securePkg_enc_in_place
  incl printf
  incl printf8
  incl printf32
  incl printMsg
  incl test

main :: IO ()
main = build
-- runCompiler [modulePkg] initialOpts { stdOut = True }

build :: IO ()
build = runCompiler [modulePkg]
  initialOpts { stdOut     = False
              , constFold  = True
              , srcDir     = "ivory-gen"
              , includeDir = "ivory-gen"
              }
