{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import Ivory.Language
import Ivory.Serialize

import SMACCMPilot.Hardware.SensorMonitor

puts :: Def('[IString]:-> Sint32)
puts = importProc "puts" "stdio.h"

main :: IO ()
main = C.compile modules artifacts
  where
  modules = decoder $ SensorHandlers
    { sh_baro   = const $ call_ puts "baro"
    , sh_mag    = const $ call_ puts "mag"
    , sh_gyro   = const $ call_ puts "gyro"
    , sh_accel  = const $ call_ puts "accel"
    , sh_gps    = const $ call_ puts "gps"
    , sh_moddef = incl puts
    }

  artifacts = makefile : runscript : serializeArtifacts

  makefile = Root $ artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -O0 -g -I. -DIVORY_TEST",
      "LDLIBS = -lm",
      "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "default: decoder",
      "\tchmod +x run.sh",
      "decoder: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
  runscript = Root $ artifactString "run.sh" $ unlines
    [ "#!/bin/sh"
    , "stty raw 115200 < $1"
    , "$(dirname $0)/decoder < $1"
    ]
