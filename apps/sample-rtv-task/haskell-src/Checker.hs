{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Checker where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.RTVerification.Operators
import Ivory.RTVerification.GenChecker
import Ivory.RTVerification.GenSettersGetters

import Variables (variables)

gettersAndHists variables

checksMod :: Module
checksMod = createModule $ properties $ do
     -- historically [get_id0] (\_ -> true)
  historically [get_id0] (\[x] -> x >? 0)
  historically [get_id1] (\[x] -> x ==? 100)

main :: IO ()
main = do
  writeCFilesForVariables variables "generated"
  runCompiler [checksMod]
    initialOpts { includeDir = "generated", srcDir = "generated" }
