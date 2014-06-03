--
-- ATIM18.hs --- STM32F4 Advanced Timer (TIM1 and TIM8) driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.ATIM18
  ( ATIM(..)
  , tim1
  , tim8
  , module Ivory.BSP.STM32.Peripheral.ATIM18.Regs
  , module Ivory.BSP.STM32.Peripheral.ATIM18.RegTypes
  ) where

import Ivory.BSP.STM32.Peripheral.ATIM18.Peripheral
import Ivory.BSP.STM32.Peripheral.ATIM18.Regs
import Ivory.BSP.STM32.Peripheral.ATIM18.RegTypes

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.MemoryMap

tim1 :: ATIM
tim1 = mkATIM tim1_periph_base
          (rccEnable rcc_apb2en_tim1)
          (rccDisable rcc_apb2en_tim1)
          "tim1"

tim8 :: ATIM
tim8 = mkATIM tim8_periph_base
          (rccEnable rcc_apb2en_tim8)
          (rccDisable rcc_apb2en_tim8)
          "tim8"

-- TIM1 and TIM8 are in APB2
rccEnable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccEnable f = modifyReg regRCC_APB2ENR $ setBit f
rccDisable :: BitDataField RCC_APB2ENR Bit -> Ivory eff ()
rccDisable f = modifyReg regRCC_APB2ENR $ clearBit f
