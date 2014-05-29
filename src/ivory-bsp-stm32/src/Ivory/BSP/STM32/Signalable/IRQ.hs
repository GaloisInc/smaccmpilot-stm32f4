
module Ivory.BSP.STM32.Signalable.IRQ where

data IRQ i = Exception Exception
           | Interrupt i


irqn :: (STM32F4Interrupt i) => IRQ i -> IRQn
irqn (Exception e) = exceptionIRQn e
irqn (Interrupt i) = interruptIRQn i


