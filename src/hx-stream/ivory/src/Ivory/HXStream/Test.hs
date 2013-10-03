{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.HXStream.Test where

import Control.Monad (replicateM)
import Data.Word
import Data.String (fromString)
import System.Directory (doesFileExist)
import Control.Monad (unless)

import Ivory.HXStream
import Ivory.Language
import Ivory.Stdlib hiding (unless)
import Ivory.Compile.C.CmdlineFrontend

import qualified Test.QuickCheck as Q

--------------------------------------------------------------------------------

puts :: Def ('[IString] :-> ())
puts = importProc "printf" "stdio.h"

testReport :: Def ('[IBool] :-> ())
testReport = proc "testReport " $ \b -> body $
  ifte_ b (c "pass") (c "fail")
  where
  c msg = call_ puts (fromString (msg ++ "\n"))

instance Q.Arbitrary Uint8 where
  arbitrary = fmap fromIntegral (Q.arbitrary :: Q.Gen Word8)

-- List that is is 128 bytes or less and an index into the list.
sampleArrayIx :: Q.Gen [Uint8]
sampleArrayIx = do
  let item = Q.frequency [(1, return 0x7e), (1,return 0x7c) ,(8, Q.arbitrary) ]
  arr <- Q.vectorOf 128 item
  return arr

runTest :: (GetAlloc eff ~ Scope s)
        => [Uint8] -> Ivory eff ()
runTest ls = do
  input   <- local $ iarray (map ival ls)
  encoded <- local $ iarray (replicate 259 (ival 0))
  decoded <- local $ iarray (replicate 128 (ival 0))
  call_ encodeTest (constRef input)   encoded
  call_ decodeTest (constRef encoded) decoded

  b <- local (ival true)
  arrayMap $ \i -> do
    x <- deref (decoded ! i)
    y <- deref (input   ! i)
    when (x /=? y) $ store b false

  b' <- deref b
  call_ testReport b'

encodeTest :: Def('[ ConstRef s0 (Array 128 (Stored Uint8))
                   , Ref s1 (Array 259 (Stored Uint8))
                   ] :-> ())
encodeTest = proc "hxstream_test_encode" $ \ pt framed -> body $ do
  writeix <- local (ival 0)
  let cb v = do
        ix <- deref writeix
        store (framed ! ix) v
        store writeix (ix + 1)
  noReturn $ encode 0 pt cb
  -- Now write start bytes into the rest of the array.
  ix <- deref writeix
  arrayMap $ \ix' -> when (ix' >=? ix) $ store (framed ! ix') fbo

decodeTest :: Def('[ ConstRef s0 (Array 259 (Stored Uint8))
                   , Ref s1 (Array 128 (Stored Uint8))
                   ] :-> ())
decodeTest = proc "hxstream_test_decode" $ \ framed pt -> body $ do
  hx <- local initStreamState
  ends <- local (ival (0::Uint32))
  let handler = mkFrameHandler ScopedFrameHandler
        { fhTag = 0
        , fhBegin = return  ()
        , fhData = \v writeoffs -> do
            let writeix = toIx writeoffs
            assert $ writeoffs <? 128
            store (pt ! writeix) v
        , fhEnd = do
            n <- deref ends
            assert (n ==? 0)
            store ends (n+1)
        }
  arrayMap $ \ix -> do
    f <- deref (framed ! ix)
    noBreak $ noReturn $ decode handler hx f

  -- There's only one frame decoded.
  totalends <- deref ends
  assert (totalends ==? 1)


test :: [[Uint8]] -> Def ('[] :-> Sint32)
test gens = proc "main" $ body $ do
    mapM_ runTest gens
    ret 0

writeMakefile :: String -> IO ()
writeMakefile pkgname = do
  e <- doesFileExist path
  unless e $ writeFile path (makefile pkgname)
  where
  path = "test/Makefile"
  makefile fname = unlines
    [ "CFLAGS += -std=c99"
    , "CFLAGS += -DIVORY_DEPLOY"
    , "CFLAGS += -g3"
    , "default: test"
    , fname ++ ": *.c"
    , "\t$(CC) $(CFLAGS) -o $@ $^"
    , "test: " ++ fname
    , "\t./" ++ fname ++ " > test_output.tmp"
    , "\t@echo \"TOTAL TESTS:\""
    , "\t@cat test_output.tmp | wc -l"
    , "\t@echo \"TOTAL SUCCESSES:\""
    , "\t@grep pass test_output.tmp | wc -l"
    , "clean:"
    , "\t-rm " ++ fname
    , "\t-rm test_output.tmp"
    ]

runTests :: IO ()
runTests = do
  vals <- replicateM 100 $ Q.sample' sampleArrayIx
  let p = test (concat vals)
  runCompiler [hxstreamModule, cmodule p]
              initialOpts { includeDir = "test"
                          , srcDir     = "test"
                          , constFold = True
                          }
  writeMakefile pkgName
  where
  pkgName = "hxstream-test"
  cmodule p = package pkgName $ do
    depend hxstreamModule
    incl p
    incl puts
    incl testReport
    incl encodeTest
    incl decodeTest
