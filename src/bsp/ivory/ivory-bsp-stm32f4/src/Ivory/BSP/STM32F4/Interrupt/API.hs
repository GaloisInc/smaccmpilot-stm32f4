
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
interrupt_enable i = return () -- XXX should use NVIC_EnableIRQ from cmsis?

interrupt_disable :: Interrupt -> Ivory eff ()
interrupt_disable i = return () -- XXX should use NVIC_EnableIRQ from cmsis?

interrupt_set_priority :: Interrupt -> Uint8 -> Ivory eff ()
interrupt_set_priority i priority = return () -- XXX
-- need NVIC->IP register
