{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import GHC.TypeLits

import qualified Data.List as L

import Ivory.Language
import Ivory.Artifact
import Ivory.Stdlib
import Ivory.Compile.C.CmdlineFrontend
import SMACCMPilot.Commsec.Ivory
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import qualified SMACCMPilot.Commsec.Ivory.Error as E
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

main :: IO ()
main = compile [ m, symmetricKeyTypesModule ] as
  where
  as = commsecArtifacts
     ++ [ Root $ artifactString "Makefile" makefile ]

pubA,pubB   :: ByteString -- PublicKey
privA,privB :: ByteString -- PrivateKey
pubA  = "\245R\181\RS\180.\251\241\255}\218\138FV\223\r\f\ETB\231v\ETBc\202O\250\"h\228\EM\137.4"
privA = "=%ZK\194\220\220\r.J\140B\220\187aGA\133\140\&9\141J\188\153\&2\197a\ENQ\ESC\191t\170"
pubB  = "2^G\ETBj\SOH\193\ETB\207\190\211\159b%%\232\US\136\176%\ETB!\214\252\233\243\151vOt\142\SYN"
privB = "\139Y\227Ct6\EOT\167\209I\232\250y\242\255\158\&1\ETX`\204\210~\DC3\SI\212\NAK\RS\217:\221m\253"

m :: Module
m = package "main" $ do
  incl main_proc
  depend symmetricKeyTypesModule
  gec_encode_moddef ce
  gec_decode_moddef cd
  gke_initiate_moddef ki
  gke_respond_moddef kr
  where
  ki = gkeInitiate "kei" pubB (pubA,privA)
  kr = gkeRespond  "ker" pubA (pubB,privB)
  ce = gecEncode "etest"
  cd = gecDecode "dtest"
  main_proc :: Def('[Sint32, Ref s (Stored (Ref s (Stored IChar)))]:->Sint32)
  main_proc = proc "main" $ \ _ _ -> body $ do
    (ks1,ks2) <- keTest
    sk1 <- local izero
    sk2 <- local izero
    assertEq ks1 ks2 200
    call_ deriveSymmetricKey (constRef ks1) sk1
    call_ deriveSymmetricKey (constRef ks2) sk2
    assertEq (sk1 ~> c2s_ks) (sk2 ~> c2s_ks) 201
    assertEq (sk1 ~> s2c_ks) (sk2 ~> s2c_ks) 202
    encDecTest (sk1 ~> c2s_ks)
    encDecTest (sk1 ~> s2c_ks)
    ret 0
  assertEq :: KnownNat n => Ref s1 (Array n (Stored Uint8)) -> Ref s2 (Array n (Stored Uint8)) -> Int -> Ivory (ProcEffects s Sint32) ()
  assertEq r1 r2 e = do
      arrayMap $ \ix -> do
          v1 <- deref (r1!ix)
          v2 <- deref (r2!ix)
          ifte_ (v1 ==? v2) (return ()) (ret (fromIntegral e))
  encDecTest :: Ref s2 SymKeySaltArray -> Ivory (ProcEffects s Sint32) ()
  encDecTest sk = do
    gec_encode_init ce (constRef sk)
    pt <- local (iarray (map (ival . fromIntegral) [0::Int,1..]))
    ct <- local (iarray [])
    e_err <- gec_encode_run ce (constRef pt) ct
    when (e_err /=? success) (ret 1)
    gec_decode_init cd (constRef sk)
    pt' <- local (iarray [])
    d_err <- gec_decode_run cd (constRef ct) pt'
    when (d_err /=? success) (ret 2)
    arrayMap $ \ix -> do
      p <- deref (pt ! ix)
      p' <- deref (pt' ! ix)
      when (p /=? p') (ret 3)
  keTest :: Ivory (ProcEffects s Sint32) (Ref (Stack s) KeyMaterial, Ref (Stack s) KeyMaterial)
  keTest = do
      m1    <- local izero
      m2    <- local izero
      m3    <- local izero
      rand1 <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      rand2 <- local (iarray (map (ival . fromIntegral) [(10::Int)..]))
      km1   <- local izero
      km2   <- local izero
      gke_initiate_init ki
      gke_respond_init kr
      e3 <- gke_initiate ki m1 (constRef rand1)
      e4 <- gke_respond kr (constRef m1) m2 (constRef rand2)
      e5 <- gke_response_ack ki (constRef m2) m3 km1
      e6 <- gke_finish kr (constRef m3) km2
      when (e3 /=? E.success) (ret 103)
      when (e4 /=? E.success) (ret 104)
      when (e5 /=? E.success) (ret 105)
      when (e6 /=? E.success) (ret 106)
      assertEq km1 km2 107
      return (km1,km2)
      -- XXX add expected failure cases

objects :: [FilePath]
objects =
  [ "aescrypt.o"
  , "aeskey.o"
  , "aestab.o"
  , "aes_modes.o"
  , "gec.o"
  , "gec-ke.o"
  , "ed25519.o"
  , "curve25519-donna.o"
  , "gcm.o"
  , "gf128mul.o"
  , "gf_convert.o"
  , "main.o"
  , "gec_symmetric_key_types.o"
  ]

makefile :: String
makefile = unlines
  [ "CC := gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O0 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parentheses \\"
  , "  -Wno-unused-variable \\"
  , "  -Wno-unused-function \\" -- XXX only because ed25519_hash, which should be switched out.
  , "  -DED25519_REFHASH \\"
  , "  -DIVORY_TEST -I."
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS) test"
  , ""
  , "test: $(OBJS)"
  , "\t$(CC) $(CFLAGS) $(OBJS) -o test"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR):"
  , "\tmkdir -p $(OBJDIR)"
  , ""
  , "clean:"
  , "\t-rm -rf obj"
  , ""
  , "veryclean: clean"
  , "\t-rm *.c *.h"
  ]

xc_makefile :: String
xc_makefile = unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parentheses \\"
  , "  -Wno-unused-variable \\"
  , "  -Wno-unused-function \\" -- XXX only because ed25519_hash, which should be switched out.
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
  , "  -DIVORY_TEST -I."
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS)"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR):"
  , "\tmkdir -p $(OBJDIR)"
  , ""
  , "clean:"
  , "\t-rm -rf obj"
  , ""
  , "veryclean: clean"
  , "\t-rm *.c *.h"
  ]

