{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Signalable.Class where

import Ivory.Tower
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.Interrupt

class (Signalable p, STM32Interrupt (InterruptType p))
      => STM32Signal p where
  type InterruptType p
  stm32Signal :: IRQ (InterruptType p) -> SignalType p

stm32Interrupt :: (STM32Signal p) => InterruptType p -> SignalType p
stm32Interrupt = stm32Signal . Interrupt
stm32Exception :: (STM32Signal p) => Exception -> SignalType p
stm32Exception = stm32Signal . Exception

