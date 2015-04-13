{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.List as L

import Ivory.Language
import Ivory.Artifact
import Ivory.Stdlib
import Ivory.Compile.C.CmdlineFrontend
import SMACCMPilot.Commsec.Ivory
import qualified SMACCMPilot.Commsec.Ivory.Error as E
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

main :: IO ()
main = compile [ m ] as
  where
  as = commsecArtifacts
     ++ [ artifactString "Makefile" makefile ]

trivial_key :: KeySalt
trivial_key = KeySalt { ks_keysalt = take 24 [1..] }

pubA,pubB   :: ByteString -- PublicKey
privA,privB :: ByteString -- PrivateKey
pubA  = "\245R\181\RS\180.\251\241\255}\218\138FV\223\r\f\ETB\231v\ETBc\202O\250\"h\228\EM\137.4"
privA = "=%ZK\194\220\220\r.J\140B\220\187aGA\133\140\&9\141J\188\153\&2\197a\ENQ\ESC\191t\170"
pubB  = "gp.5\166\EOT\f\166\NAK\158mj\165\158\146B\174\221!4\STX[nY\220\147\198\210\141\252\183 "
privB = "#yyE\130\187_\158\SUB\202\v\153\128\252\169\245)\179\178Z\133+\242p\163Ol+\174\STX\238\US"

m :: Module
m = package "main" $ do
  incl main_proc
  gec_encode_moddef ce
  gec_decode_moddef cd
  gke_initiate_moddef ki
  gke_respond_moddef kr
  where
  ki = gkeInitiate "kei" pubB (pubA,privA)
  kr = gkeRespond  "ker" pubA (pubB,privB)
  ce = gecEncode trivial_key "etest"
  cd = gecDecode trivial_key "dtest"
  main_proc :: Def('[Sint32, Ref s (Stored (Ref s (Stored IChar)))]:->Sint32)
  main_proc = proc "main" $ \ _ _ -> body $ do
    encDecTest
    keTest
    ret 0
  encDecTest :: Ivory (ProcEffects s Sint32) ()
  encDecTest = do
    gec_encode_init ce
    pt <- local (iarray (map (ival . fromIntegral) [0::Int,1..]))
    ct <- local (iarray [])
    e_err <- gec_encode_run ce (constRef pt) ct
    when (e_err /=? success) (ret 1)
    gec_decode_init cd
    pt' <- local (iarray [])
    d_err <- gec_decode_run cd (constRef ct) pt'
    when (d_err /=? success) (ret 2)
    arrayMap $ \ix -> do
      p <- deref (pt ! ix)
      p' <- deref (pt' ! ix)
      when (p /=? p') (ret 3)
  keTest :: Ivory (ProcEffects s Sint32) ()
  keTest = do
      m1    <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      m2    <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      m3    <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      rand1 <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      rand2 <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      km1   <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
      km2   <- local (iarray (map (ival . fromIntegral) [(0::Int)..]))
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
      arrayMap $ \ix -> do
          k  <- deref (km1 ! ix)
          k' <- deref (km2 ! ix)
          when (k /=? k') (ret 4)
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

