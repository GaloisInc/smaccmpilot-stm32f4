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

-- Generates all the setters and getters using TH.
gettersAndHists

checksMod :: Module
checksMod = createModule $ properties $ do
     -- historically [get_id0] (\_ -> true)
  historically [get_id0] (\[x] -> x >? 0)
  historically [get_id1] (\[x] -> x ==? 100)

main :: IO ()
main = do
  writeCFilesForVariables "generated"
  runCompiler [checksMod]
    initialOpts { includeDir = "generated", srcDir = "generated" }
