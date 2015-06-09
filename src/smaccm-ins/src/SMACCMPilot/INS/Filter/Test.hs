{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SMACCMPilot.INS.Filter.Test
  ( app
  ) where

import Data.List (intercalate)
import System.FilePath

import Ivory.Language
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import SMACCMPilot.INS.Filter

app :: IO ()
app = C.compile modules artifacts
  where

  modules = [filter_test_pkg]
  artifacts = [makefile]

  objects = [ moduleName m <.> "o" | m <- modules ]

  exename = moduleName $ head modules

  makefile = Root $ artifactString "Makefile" $ unlines
    [ "CC = gcc"
    , "CFLAGS = -Wall -g -Os -std=c99 -D_BSD_SOURCE -Wno-unused-variable -I."
    , "LDLIBS = -lm"
    , "OBJS = " ++ intercalate " " objects
    , exename ++ ": $(OBJS)"
    , "clean:"
    , "\t-rm -f $(OBJS) " ++ exename
    , ".PHONY: clean"
    ]



filter_test_pkg :: Module
filter_test_pkg = package "filter_test" $ do
  incl main_proc
  lpf_moddef
  hpf_moddef
  avg_moddef
  incl printf_float
  incl puts
  incl sin_proc
  where
  (lpf, lpf_moddef) = ivory2ndOrderFilter "lpf" lowPassButterworth
  (hpf, hpf_moddef) = ivory2ndOrderFilter "hpf" highPassButterworth
  (avg, avg_moddef) = ivoryRunningAverageFilter "avg" (Proxy :: Proxy 64)

  put_float :: IFloat -> Ivory eff ()
  put_float = call_ printf_float "%f\t"
  endl = call_ puts ""


  test :: (Ix 100 -> Ivory (AllowBreak eff) IFloat) -> Ivory eff ()
  test f = do
    filter_init lpf
    filter_init hpf
    filter_init avg
    arrayMap $ \ (ix :: Ix 100) -> do
      input <- f ix
      put_float input
      filter_sample lpf input
      filter_out lpf >>= put_float
      filter_sample hpf input
      filter_out hpf >>= put_float
      filter_sample avg input
      filter_out avg >>= put_float
      endl

  main_proc :: Def('[]:->Sint32)
  main_proc = proc "main" $ body $ do
    call_ puts "input lpf hpf avg"
    -- Impulse function
    test (\ix -> return ((ix ==? 0) ? (1, 0)))
    -- Step function
    test (const (return 1))
    -- Alternating function
    test (\ix -> return (((fromIx ix) .% 2 ==? 0) ? (1, 0)))

    ret 0


printf_float :: Def('[IString, IFloat] :-> ())
printf_float = importProc "printf" "stdio.h"

puts :: Def('[IString] :-> ())
puts = importProc "puts" "stdio.h"

sin_proc :: Def('[IFloat] :-> IFloat)
sin_proc = importProc "sin" "math.h"
