{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32F405.Signalable.Class
  ( STM32F4Signal(..)
  , stm32f4Interrupt
  , stm32f4Exception
  ) where

import Ivory.Tower
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32F405.Interrupt

class (Signalable a) => STM32F4Signal a where
  stm32f4Signal :: IRQ -> SignalType a

stm32f4Interrupt :: (STM32F4Signal a) => Interrupt -> SignalType a
stm32f4Interrupt = stm32f4Signal . Interrupt
stm32f4Exception :: (STM32F4Signal a) => Exception -> SignalType a
stm32f4Exception = stm32f4Signal . Exception
