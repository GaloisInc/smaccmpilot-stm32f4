
module Ivory.BSP.STM32F405.VectorTable
  ( vector_table
  ) where

import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32F405.Interrupt

reset_handler :: String
reset_handler = exceptionHandlerName Reset

vector_table :: String
vector_table = unlines $
  [ ""
  , ""
  , "\t.syntax unified"
  , "\t.thumb"
  , "\t.cpu cortex-m3"
  , "\t.fpu softvfp"
  , ""
  , ""
  , ".global " ++ reset_handler
  , ".global g_vectors"
  , ".word _sidata"
  , ".word _sdata"
  , ".word _edata"
  , ".word _sbss"
  , ".word _ebss"
  , ""
  , ""
  , "\t.section .text.defaultExceptionHandler, \"ax\",%progbits"
  , "defaultExceptionHandler:"
  , "\tb defaultExceptionHandler"
  , "\t.size defaultExceptionHandler, .-defaultExceptionHandler"
  , ""
  , "\t.section .isr_vector,\"ax\""
  , "\t.code    16"
  , "\t.align   2"
  , "\t.globl   g_vectors"
  , "\t.type    g_vectors, function"
  , ""
  , "g_vectors:"
  , ""
  , "\t.word _estack"
  ] ++
  map (entry . (fmap exceptionHandlerName)) exceptionTable ++
  map (entry . (fmap interruptHandlerName)) interruptTable ++
  [ ""
  , "\t.size g_vectors, .-g_vectors"
  , ""
  ] ++
  map (weakdef . (fmap exceptionHandlerName)) (drop 1 exceptionTable) ++
  map (weakdef . (fmap interruptHandlerName)) interruptTable

  where
  entry (Just e) = "\t.word " ++ e
  entry Nothing  = "\t.word 0"
  weakdef (Just handler) = "\t.weak " ++ handler ++
    "\n\t.thumb_set " ++ handler ++ ",defaultExceptionHandler\n"
  weakdef Nothing = ""


