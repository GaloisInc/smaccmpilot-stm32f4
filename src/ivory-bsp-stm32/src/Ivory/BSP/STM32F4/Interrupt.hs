
module Ivory.BSP.STM32F4.Interrupt
  -- from Types:
  ( IRQ(..), irqn, irqs
  , irqHandlerName
  , irqFromHandlerName
  , Exception(..), exceptionIRQn, exceptionHandlerName
  , Interrupt(..), interruptIRQn, interruptHandlerName
  -- from API:
  , interrupt_enable
  , interrupt_disable
  , interrupt_set_pending
  , interrupt_clear_pending
  , interrupt_set_priority
  , interrupt_set_to_syscall_priority
  ) where

import Ivory.BSP.STM32F4.Interrupt.Types
import Ivory.BSP.STM32F4.Interrupt.API

import Ivory.Language

irqHandlerName :: IRQ -> String
irqHandlerName (Exception e) = exceptionHandlerName e
irqHandlerName (Interrupt i) = interruptHandlerName i

irqFromHandlerName :: String -> IRQ
irqFromHandlerName = error "irqFromHandlerName is a stub!" -- XXX

-- Based on convention used in init/startup_stm43f4xx.s
exceptionHandlerName :: Exception -> String
exceptionHandlerName e = (show e) ++ "_IRQHandler"

interruptHandlerName :: Interrupt -> String
interruptHandlerName i = (show i) ++ "_IRQHandler"

interrupt_set_to_syscall_priority :: Interrupt -> Ivory eff ()
interrupt_set_to_syscall_priority i =
  interrupt_set_priority i max_syscall_priority
  -- XXX MAGIC NUMBER: in tower/freertos, syscalls must be lower (numerically greater
  -- than) level 11. XXX how to make this cross OS platform correctly?
  where max_syscall_priority = (12::Uint8)

