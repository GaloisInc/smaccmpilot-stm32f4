{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.HXStream.Test where

import Control.Monad (replicateM, unless)
import Data.Word
import Data.String (fromString)
import System.Directory (doesFileExist)

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
  call_ encode (constRef input)   encoded
  call_ decode (constRef encoded) decoded

  b <- local (ival true)
  arrayMap $ \i -> do
    x <- deref (decoded ! i)
    y <- deref (input   ! i)
    when (x /=? y) $ store b false

  b' <- deref b
  call_ testReport b'

encode :: Def('[ ConstRef s (Array 128 (Stored Uint8))
               , Ref ss (Array 259 (Stored Uint8))
               ] :-> ())
encode = proc "hxstream_test_encode" $ \ pt framed -> body $ do
  writeix <- local (ival 0)
  let cb v = do
        ix <- deref writeix
        store (framed ! ix) v
        store writeix (ix + 1)
  noReturn $ encodeToBS 0 pt cb

decode :: Def('[ ConstRef s (Array 259 (Stored Uint8))
               , Ref ss (Array 128 (Stored Uint8))
               ] :-> ())
decode = proc "hxstream_test_decode" $ \ framed pt -> body $ do
  hx <- local hx_ival
  runs <- local (ival (0::Uint32))
  ends <- local (ival (0::Uint32))
  let handler = FrameHandler $ ScopedFrameHandler
        { fh_tag = 0
        , fh_begin = do
            n <- deref runs
            assert (n ==? 0)
            store runs (n+1)
        , fh_data = \v writeoffs -> do
            let writeix = toIx writeoffs
            assert $ writeoffs <? 128
            store (pt ! writeix) v
        , fh_end = do
            n <- deref ends
            assert (n ==? 0)
            store ends (n+1)
        }
  arrayMap $ \ix -> do
    f <- deref (framed ! ix)
    noBreak $ noReturn $ decodeSM [handler] hx f

  totalruns <- deref runs
  assert (totalruns ==? 1)
  totalends <- deref ends
  assert (totalends ==? 1)


test :: [[Uint8]] -> Def ('[] :-> Sint32)
test gens = proc "main" $ body $ do
    mapM_ runTest gens
    ret 0

main :: IO ()
main = do
  vals <- replicateM 100 $ Q.sample' sampleArrayIx
  let p = test (concat vals)
  runCompiler [cmodule p]
              initialOpts { includeDir = "test"
                          , srcDir     = "test"
                          , constFold = True
                          }
  writeMakefile pkgname
  where
  pkgname = "hxstream-test"
  cmodule p = package "hxstream-test" $ do
    incl p
    defStruct (Proxy :: Proxy "hxstream_state")
    incl puts
    incl testReport
    incl encode
    incl decode


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
    , fname ++ ": " ++ fname ++ ".c"
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
