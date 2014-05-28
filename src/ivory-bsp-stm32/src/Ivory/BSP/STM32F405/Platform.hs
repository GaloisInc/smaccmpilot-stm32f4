{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32F405.Platform
  ( STM32F405(..)
  ) where

import qualified Ivory.BSP.STM32.Platform as STM32

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral as GPIOF4

data STM32F405 = STM32F405

instance STM32.Platform STM32F405 where
  data Interrupt STM32F405 = STM32F405Interrupt F405.Interrupt
  data GPIOPin   STM32F405 = STM32F405GPIO      GPIOF4.GPIOPin

