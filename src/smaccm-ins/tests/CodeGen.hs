import Data.List
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import Ivory.Language
import IvoryFilter
import qualified Paths_smaccm_ins as P

main :: IO ()
main = C.compile modules artifacts
  where
  modules = [ins_module]
  artifacts = makefile : map (artifactCabalFile P.getDataDir) ["tests/psas.c", "tests/psas-packet.h"]
  makefile = artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -Os",
      "LDLIBS = -lm",
      "OBJS = psas.o " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "psas: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
