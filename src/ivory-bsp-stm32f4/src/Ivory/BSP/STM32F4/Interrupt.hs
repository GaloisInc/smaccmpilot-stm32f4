
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
  , interrupt_set_priority
  ) where

import Ivory.BSP.STM32F4.Interrupt.Types
import Ivory.BSP.STM32F4.Interrupt.API

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

