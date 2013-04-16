{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.RTVerification.Operators
import Ivory.RTVerification.GenChecker

get_var0 :: Getter Sint32
get_var0 = importProc "get_var0" "instrumented"

get_var1 :: Getter Sint32
get_var1 = importProc "get_var1" "instrumented"

checksMod :: Module
checksMod = createModule $ properties $ do
  historically [get_var0] (\[x] -> x >? 0)
  historically [get_var1] (\[x] -> x ==? 100)

main :: IO ()
main = do
  runCompiler [checksMod] initialOpts { includeDir = "generated", srcDir = "generated" }
