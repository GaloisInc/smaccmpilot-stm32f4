{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ivory.HXStream.Test where

import Data.Word

import Ivory.HXStream
import Ivory.Language
import Ivory.Stdlib
import Ivory.Compile.C.CmdlineFrontend

import qualified Test.QuickCheck as Q

import qualified SMACCMPilot.Shared as S

--------------------------------------------------------------------------------

pf :: Def ('[IString, IBool] :-> ())
pf = importProc "printf" "stdio.h"

pr :: Def ('[IBool] :-> ())
pr = proc "pr" $ \b -> body $
  call_ pf "%i\n" b

instance Q.Arbitrary Uint8 where
  arbitrary = fmap fromIntegral (Q.arbitrary :: Q.Gen Word8)

-- List that is commsec sized bytes or less and an index into the list.
sampleArrayIx :: Q.Gen ([Uint8], Uint8)
sampleArrayIx = do
  sz  <- Q.choose (1::Integer, S.commsecPkgSize)
  arr <- Q.resize (fromInteger S.commsecPkgSize) Q.arbitrary
  return (arr, fromIntegral sz)

runTest :: (GetAlloc eff ~ Scope s)
        => ([Uint8], Uint8) -> Ivory eff ()
runTest (ls, _) = do
  st <- local (istruct [])
  emptyStreamState st
  input  <- local $ iarray (map ival ls)
  output <- local $ iarray (replicate (fromInteger S.hxstreamPkgSize) (ival 0))
  call_ encode input output
  call_ decode output st

  b <- local (ival true)
  arrayMap $ \i -> do
    let arr = st ~> buf
    x <- deref (arr ! i)
    y <- deref (input ! i)
    unless (x ==? y)
           (store b false)

  b' <- deref b
  call_ pr b'

test :: [([Uint8], Uint8)] -> Def ('[] :-> Sint32)
test gens = proc "main" $ body $ do
    mapM_ runTest gens
    ret 0

main :: IO ()
main = do
  vals <- Q.sample' sampleArrayIx
  let p = test vals
  runCompiler [cmodule p]
              initialOpts { includeDir = "test"
                          , srcDir     = "test"
                          , constFold = True
                          }
  where
  cmodule p = package "hxstream-test" $ do
    incl p
    defStruct (Proxy :: Proxy "hxstream_state")
    incl pf
    incl pr
    incl encode
    incl (decode :: Def ( '[ Ref s S.CommsecArray
                           , Ref s Hx
                           ] :-> S.CommsecIx))
