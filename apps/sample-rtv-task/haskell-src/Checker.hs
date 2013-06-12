{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Checker where

import Ivory.Language

import Ivory.RTVerification.Operators
import Ivory.RTVerification.GenChecker
import Ivory.RTVerification.GenSettersGetters

import System.Directory
import System.FilePath

-- Generates all the setters and getters using TH.
gettersAndHists

checksMod :: Module
checksMod = createModule $ properties $ do
     -- historically [get_id0] (\_ -> true)
  historically [get_id0] (\[x] -> x >? 0)
  historically [get_id1] (\[x] -> x ==? 100)

-- out :: String
-- out = "rv-gen"

checker :: IO ()
checker = do
  dir <- getCurrentDirectory
  writeCFilesForVariables $ dir </> "apps/sample-rtv-task" </> "checker"

-- checker :: IO ()
-- checker = do
--   writeCFilesForVariables out
--   C.compileWith [checksMod]
--   runCompiler [checksMod] initialOpts { includeDir = out
--                                       , srcDir = out
--                                       }
