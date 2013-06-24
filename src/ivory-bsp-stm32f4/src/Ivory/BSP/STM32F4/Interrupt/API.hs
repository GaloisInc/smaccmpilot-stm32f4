--
-- API.hs --- Interrupt Controller API
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.Interrupt.API where

import Ivory.BSP.STM32F4.Interrupt.Types
import Ivory.BSP.STM32F4.Interrupt.Regs

import Ivory.Language
import Ivory.BitData
import Ivory.HW


-- Based on convention used in init/startup_stm43f4xx.s
handlerName :: Interrupt -> String
handlerName i = (show i) ++ "_IRQHandler"

-- | The STM32F4 NVIC ignores writes to the low 4 bits of the
-- interrupt priority registers.  We hide this from callers, so the
-- API accepts interrupt priority levels from 0 to 15.
nvic_prio_shift :: Int
nvic_prio_shift = 4

----------------------------------------------------------------------
-- High Level Interface

interrupt_enable :: Interrupt -> Ivory eff ()
interrupt_enable i = do
  let (reg, bitN) = nvic_ISER_int i
  setReg reg $ do
    setBit (nvic_iser_setena #> bitIx bitN)

interrupt_disable :: Interrupt -> Ivory eff ()
interrupt_disable i = do
  let (reg, bitN) = nvic_ICER_int i
  setReg reg $ do
    setBit (nvic_icer_clrena #> bitIx bitN)

interrupt_set_priority :: Interrupt -> Uint8 -> Ivory eff ()
interrupt_set_priority i pri = do
  let pri' = pri `iShiftR` fromIntegral nvic_prio_shift
  writeReg (nvic_IPR_int i) pri'
