{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Signalable.Class where

import Ivory.Tower
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.Interrupt

class (Signalable a, STM32Interrupt i) => STM32Signal i a | a -> i where
  stm32Signal :: IRQ (i) -> SignalType a

stm32Interrupt :: (STM32Signal i a) => i -> SignalType a
stm32Interrupt = stm32Signal . Interrupt
stm32Exception :: (STM32Signal i a) => Exception -> SignalType a
stm32Exception = stm32Signal . Exception

