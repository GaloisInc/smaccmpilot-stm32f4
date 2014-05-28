
module Ivory.BSP.STM32F405.Flash
  ( Flash(..)
  , module Ivory.BSP.STM32.Peripheral.Flash.Regs
  , flash
  ) where

import Ivory.BSP.STM32.Peripheral.Flash.Regs
import Ivory.BSP.STM32.Peripheral.Flash.Peripheral

import Ivory.BSP.STM32F405.MemoryMap (flash_r_periph_base)

flash :: Flash
flash = mkFlash flash_r_periph_base
