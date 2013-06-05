{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

--
-- API.hs --- Interrupt Controller API
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.Interrupt.API where

import Ivory.BSP.STM32F4.Interrupt.Types

import Ivory.Language

-- Based on convention used in init/startup_stm43f4xx.s
handlerName :: Interrupt -> String
handlerName i = (show i) ++ "_IRQHandler"


interrupt_enable :: Interrupt -> Ivory eff ()
interrupt_enable i = call_ extern_enable (fromIntegral (interruptIRQn i))
  where
  -- Wrapper on core_cm4 NVIC_EnableIRQ
  extern_enable :: Def ('[Sint16]:->())
  extern_enable = importProc "interrupt_num_enable"
                             "ivory_stm32f4_interrupt.h"

interrupt_disable :: Interrupt -> Ivory eff ()
interrupt_disable i = call_ extern_disable (fromIntegral (interruptIRQn i))
  where
  -- Wrapper on core_cm4 NVIC_DisableIRQ
  extern_disable :: Def ('[Sint16]:->())
  extern_disable = importProc "interrupt_num_disable"
                              "ivory_stm32f4_interrupt.h"

interrupt_set_priority :: Interrupt -> Uint8 -> Ivory eff ()
interrupt_set_priority i priority =
  call_ extern_set_priority (fromIntegral (interruptIRQn i)) priority
  where
  -- Wrapper on core_cm4 NVIC_SetPriority
  extern_set_priority :: Def ('[Sint16, Uint8]:->())
  extern_set_priority = importProc "interrupt_num_set_priority"
                                   "ivory_stm32f4_interrupt.h"

