{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Signalable.Class where

import Ivory.Language
import Ivory.Tower
import Ivory.BSP.ARMv7M.Exception

class (Signalable a) => STM32Signal a where
  type STM32Interrupt
  stm32Signal :: IRQ (STM32Interrupt a) -> SignalType a
  interruptIRQn             :: STM32Interrupt a -> IRQn
  interruptHandlerName      :: STM32Interrupt a -> String
  interrupt_enable          :: forall eff . STM32Interrupt a -> Ivory eff ()
  interrupt_disable         :: forall eff . STM32Interrupt a -> Ivory eff ()
  interrupt_set_pending     :: forall eff . STM32Interrupt a -> Ivory eff ()
  interrupt_clear_pending   :: forall eff . STM32Interrupt a -> Ivory eff ()
  interrupt_set_priority    :: forall eff . STM32Interrupt a -> Uint8 -> Ivory eff ()


stm32Interrupt :: (STM32Signal a) => STM32Interrupt a -> SignalType a
stm32Interrupt = stm32Signal . Interrupt
stm32Exception :: (STM32Signal a) => Exception -> SignalType a
stm32Exception = stm32Signal . Exception

data IRQ i = Exception Exception
           | Interrupt i

interrupt_set_to_syscall_priority :: (STM32Signal a) => (STM32Interrupt a) -> Ivory eff ()
interrupt_set_to_syscall_priority i =
  interrupt_set_priority i max_syscall_priority
  -- XXX MAGIC NUMBER: in tower/freertos, syscalls must be lower (numerically greater
  -- than) level 11. XXX how to make this cross OS platform safely?
  where max_syscall_priority = (12::Uint8)

irqn :: (STM32Signal i) => IRQ (STM32Interrupt i) -> IRQn
irqn (Exception e) = exceptionIRQn e
irqn (Interrupt i) = interruptIRQn i

irqHandlerName :: (STM32Signal i) => IRQ (STM32Interrupt i) -> String
irqHandlerName (Exception e) = exceptionHandlerName e
irqHandlerName (Interrupt i) = interruptHandlerName i

irqFromHandlerName :: (STM32Signal i) => String -> Maybe (IRQ (STM32Interrupt i))
irqFromHandlerName = error "irqFromHandlerName is a stub!" -- XXX FIXME

