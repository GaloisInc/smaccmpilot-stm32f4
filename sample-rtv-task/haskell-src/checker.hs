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

checksMod :: Module
checksMod = createModule $ properties $ do
  historically [get_var0] (\[x] -> x >? 0)

main :: IO ()
main = do
  runCompiler [checksMod] initialOpts { includeDir = "generated", srcDir = "generated" }
