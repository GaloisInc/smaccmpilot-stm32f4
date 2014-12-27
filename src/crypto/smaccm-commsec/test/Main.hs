
module Main where

import qualified Data.List as L

import Ivory.Artifact
import Ivory.Compile.C.CmdlineFrontend
import SMACCMPilot.Commsec.Artifacts

main :: IO ()
main = compile [] as
  where
  as = commsecArtifacts
     ++ [ artifactString "Makefile" makefile ]

objects :: [FilePath]
objects =
  [ "aescrypt.o"
  , "aeskey.o"
  , "aestab.o"
  , "commsec.o"
  , "gcm.o"
  , "gf128mul.o"
  , "gf_convert.o"
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

