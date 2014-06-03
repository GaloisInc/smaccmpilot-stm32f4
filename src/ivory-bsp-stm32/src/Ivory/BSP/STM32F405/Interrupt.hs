{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32F405.Interrupt
  -- from Types:
  ( F405.Interrupt(..)
  , F405.interruptIRQn
  , F405.interruptHandlerName
  , F405.interruptTable
  -- from API:
  , F405.interrupt_enable
  , F405.interrupt_disable
  , F405.interrupt_set_pending
  , F405.interrupt_clear_pending
  , F405.interrupt_set_priority
  ) where

import qualified Ivory.BSP.STM32F405.Interrupt.Types as F405
import qualified Ivory.BSP.STM32F405.Interrupt.API   as F405
import Ivory.BSP.STM32.Interrupt

instance STM32Interrupt F405.Interrupt where
  interruptIRQn = F405.interruptIRQn
  interruptHandlerName = F405.interruptHandlerName
  interrupt_enable = F405.interrupt_enable
  interrupt_disable = F405.interrupt_disable
  interrupt_set_pending = F405.interrupt_set_pending
  interrupt_clear_pending = F405.interrupt_clear_pending
  interrupt_set_priority = F405.interrupt_set_priority
