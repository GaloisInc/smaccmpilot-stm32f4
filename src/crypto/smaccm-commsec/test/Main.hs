{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.List as L

import Ivory.Language
import Ivory.Artifact
import Ivory.Compile.C.CmdlineFrontend
import SMACCMPilot.Commsec.Artifacts
import SMACCMPilot.Commsec.Module
import SMACCMPilot.Commsec.Config

main :: IO ()
main = compile [ encmod, decmod ] as
  where
  as = commsecArtifacts
     ++ [ artifactString "Makefile" makefile ]

config :: Config
config = Config
  { encode_id = 1
  , encode_key = take 16 [1..]
  , encode_salt = 0xdeadbeef
  , decode_key  = take 16 [2..]
  , decode_salt = 0xcafed00d
  }

encmod :: Module
encmod = package "test_encode" $ do
  incl test_encode_proc
  commsec_encode_moddef ce
  where
  ce = commsecEncode config "etest"
  test_encode_proc :: Def('[]:->())
  test_encode_proc = proc "test_encode" $ body $ do
    commsec_encode_init ce
    ct <- local (iarray [])
    pt <- local (iarray [])
    _e <- commsec_encode_run ce (constRef pt) ct
    return ()

decmod :: Module
decmod = package "test_decode" $ do
  incl test_decode_proc
  commsec_decode_moddef cd
  where
  cd = commsecDecode config "dtest"
  test_decode_proc :: Def('[]:->())
  test_decode_proc = proc "test_decode" $ body $ do
    commsec_decode_init cd
    ct <- local (iarray [])
    pt <- local (iarray [])
    _e <- commsec_decode_run cd (constRef ct) pt
    return ()


objects :: [FilePath]
objects =
  [ "aescrypt.o"
  , "aeskey.o"
  , "aestab.o"
  , "commsec.o"
  , "gcm.o"
  , "gf128mul.o"
  , "gf_convert.o"
  , "test_encode.o"
  , "test_decode.o"
  ]

makefile :: String
makefile = unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parentheses \\"
  , "  -Wno-unused-variable \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
  , "  -DIVORY_TEST \\"
  , "  -DARM"
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

