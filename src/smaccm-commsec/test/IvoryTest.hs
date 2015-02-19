{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.List as L

import Ivory.Language
import Ivory.Artifact
import Ivory.Stdlib
import Ivory.Compile.C.CmdlineFrontend
import SMACCMPilot.Commsec.Ivory

main :: IO ()
main = compile [ m ] as
  where
  as = commsecArtifacts
     ++ [ artifactString "Makefile" makefile ]

trivial_key :: KeySalt
trivial_key = KeySalt { ks_key = take 16 [1..], ks_salt = 0xdeadbeef }

m :: Module
m = package "main" $ do
  incl main_proc
  commsec_encode_moddef ce
  commsec_decode_moddef cd
  where
  ce = commsecEncode trivial_key "etest"
  cd = commsecDecode trivial_key "dtest"
  main_proc :: Def('[Sint32, Ref s (Stored (Ref s (Stored IChar)))]:->Sint32)
  main_proc = proc "main" $ \ _ _ -> body $ do
    commsec_encode_init ce
    pt <- local (iarray (map (ival . fromIntegral) [0::Int,1..]))
    ct <- local (iarray [])
    e_err <- commsec_encode_run ce (constRef pt) ct
    when (e_err /=? success) (ret 1)
    commsec_decode_init cd
    pt' <- local (iarray [])
    d_err <- commsec_decode_run cd (constRef ct) pt'
    when (d_err /=? success) (ret 2)
    arrayMap $ \ix -> do
      p <- deref (pt ! ix)
      p' <- deref (pt' ! ix)
      when (p /=? p') (ret 3)
    ret 0

objects :: [FilePath]
objects =
  [ "aescrypt.o"
  , "aeskey.o"
  , "aestab.o"
  , "commsec.o"
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

