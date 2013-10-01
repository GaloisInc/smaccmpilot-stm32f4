{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Testing module for HXStream protocol.

module Ivory.HXStream.Test (runTests) where

import Control.Monad (replicateM, unless)
import Data.Word
import Data.String (fromString)
import System.Directory (doesFileExist)

import Ivory.HXStream
import Ivory.Language
import Ivory.Stdlib hiding (unless)

import Ivory.Compile.C.CmdlineFrontend

import qualified Test.QuickCheck    as Q
import qualified SMACCMPilot.Shared as S

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

-- List that is is commsec-sized bytes or less and an index into the list.
sampleArrayIx :: Q.Gen [Uint8]
sampleArrayIx = do
  let item = Q.frequency [(1, return 0x7e), (1,return 0x7c) ,(8, Q.arbitrary) ]
  Q.vectorOf (fromInteger S.commsecPkgSize) item

runTest :: (GetAlloc eff ~ Scope s)
        => [Uint8] -> Ivory eff ()
runTest ls = do
  input   <- local $ iarray (map ival ls)
  encoded <- local $ iarray (replicate (fromInteger S.hxstreamPkgSize) (ival 0))
  decoded <- local $ iarray (replicate (fromInteger S.commsecPkgSize) (ival 0))
  call_ encode 0 input encoded -- tag value doesn't matter
  _ <- call decode (constRef encoded) decoded

  b <- local (ival true)
  arrayMap $ \i -> do
    x <- deref (decoded ! i)
    y <- deref (input   ! i)
    when (x /=? y)  $ store b false

  b' <- deref b
  call_ testReport b'

-- | Decode an hxstreamed array into the raw bytes.  Decoding stops if (1) the
-- hxstream state buffer we're decoding into fills (2) a stop byte is
-- encountered (the input array may contain hxstream frame boundaries), (3) we
-- run out of array to decode from.  An index to the first byte not decoded is
-- returned.  The index may overflow, and point to 0 if the full input array is
-- processed (at which point, (state ~> hxstate_idle) should be true).
decode :: Def ( '[ ConstRef s0 S.HxstreamArray
                 , Ref      s1 S.CommsecArray
                 ] :-> S.HxstreamIx)
decode = proc "decode" $ \from to -> body $ do
  state <- local $ istruct []
  emptyStreamState state
  arrayMap $ \ix -> do
    v    <- deref (from ! ix)
    b    <- call decodeSM state to v
    over <- deref (state ~> ovf)
    -- Couldn't put the current byte in---we overflowed.
    when over (ret ix)
    -- Won't put the next byte in---we're done.
    when b (ret (ix+1))
  -- Return index 0 if all went well.
  ret 0

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

runTests :: IO ()
runTests = do
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
    incl decodeSMCommsec
