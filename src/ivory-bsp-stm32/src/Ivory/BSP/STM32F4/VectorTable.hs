
module Ivory.BSP.STM32F4.VectorTable
  ( vector_table
  ) where

import Ivory.BSP.STM32F4.Interrupt

vector_table :: String
vector_table = unlines $
  [ ""
  , ""
  , "\t.syntax unfiied"
  , "\t.thumb"
  , "\t.cpu cortex-m3"
  , "\t.fpu softvfp"

  , ".global ResetException"
  , ".global g_vectors"
  , ".word _sidata"
  , ".word _sdata"
  , ".word _edata"
  , ".word _sbss"
  , ".word _ebss"

  , "\t.section .text.defaultExceptionHandler, \"ax\",%progbits"
  , "defaultExceptionHandler:"
  , "\tb defaultExceptionHandler"
  , "\t.size defaultExceptionHandler, .-defaultExceptionHandler"
  , ""
  , "\t.section .vectors \"ax\""
  , "\t.code    16"
  , "\t.align   2"
  , "\t.globl   g_vectors"
  , "\t.type    g_vectors, function"
  , ""
  , "g_vectors:"
  , ""
  , entry "(_estack+0x400)"
  , entry "ResetException"
  ] ++
  map (entry . exceptionHandlerName) (enumFrom NonMaskable) ++
  map (entry . interruptHandlerName) (enumFrom WWDG) ++
  [ ""
  , "\t.size stm32f4_vectors, .-stm32f4_vectors"
  , ""
  ] ++
  map (weakdef . exceptionHandlerName) (enumFrom NonMaskable) ++
  map (weakdef . interruptHandlerName) (enumFrom WWDG)

  where
  entry e = "\t.word " ++ e
  weakdef handler = "\t.weak " ++ handler ++
    "\n\t.thumb_set " ++ handler ++ ",defaultExceptionHandler\n"


