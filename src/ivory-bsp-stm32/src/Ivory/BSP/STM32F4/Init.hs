{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.BSP.STM32F4.Init where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32F4.VectorTable

stm32f4InitModule :: Module
stm32f4InitModule = package "stm32f4_ivory_init" $ do
  inclHeader "stm32f4_init.h"
  sourceDep  "stm32f4_init.h"
  sourceDep  "stm32f4_init.c"
  incl reset_handler
  private $ do
    incl init_clocks
    incl init_relocate
    incl init_libc
    incl main

stm32f4InitTower :: Tower p ()
stm32f4InitTower = do
  towerArtifact vectorArtifact
  towerModule stm32f4InitModule
  where
  vectorArtifact = Artifact
    { artifact_filepath = "stm32f4_vectors.s"
    , artifact_contents = vector_table
    , artifact_tag      = "SOURCES"
    }

init_relocate :: Def('[]:->())
init_relocate = externProc "init_relocate"

init_libc :: Def('[]:->())
init_libc = externProc "init_libc"

main :: Def('[]:->())
main = externProc "main"

reset_handler :: Def('[]:->())
reset_handler = proc (exceptionHandlerName Reset) $ body $ do
  call_ init_relocate
  call_ init_clocks
  call_ init_libc
  call_ main

init_clocks :: Def('[]:->())
init_clocks = proc "init_clocks" $ body $ do
  return () -- XXX

