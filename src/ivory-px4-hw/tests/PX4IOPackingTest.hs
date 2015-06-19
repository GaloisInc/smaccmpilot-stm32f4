
module Main where

import Data.List
import Ivory.Language
import Ivory.Artifact
import SMACCMPilot.Hardware.Tests.PX4IOPacking (app)
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import System.FilePath

main :: IO ()
main = C.compile modules (makefile : artifacts)
  where
  (modules, artifacts) = app

  exename = moduleName $ head modules

  objects = [ moduleName m <.> "o" | m <- modules ] ++
    [ replaceExtension (artifactFileName a) "o" | Src a <- artifacts ]

  makefile = Root $ artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -O0 -g -I. -DIVORY_TEST",
      "OBJS = " ++ intercalate " " objects,
      exename ++ ": $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS) " ++ exename,
      ".PHONY: clean"
    ]
