--
-- GTIM2345.hs --- General Purpose Timer (TIM2 to TIM5) driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.GTIM2345
  ( GTIM(..)
  , GTIM32
  , GTIM16
  , tim2
  , tim2_32
  , tim3
  , tim4
  , tim5
  , tim5_32
  , module Ivory.BSP.STM32.Peripheral.GTIM2345.Regs
  , module Ivory.BSP.STM32.Peripheral.GTIM2345.RegTypes
  ) where

import Ivory.BSP.STM32.Peripheral.GTIM2345.Peripheral
import Ivory.BSP.STM32.Peripheral.GTIM2345.Regs
import Ivory.BSP.STM32.Peripheral.GTIM2345.RegTypes

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.MemoryMap

tim2 :: GTIM16
tim2 = mkGTIM tim2_periph_base
          (rccEnable rcc_apb1en_tim2)
          (rccDisable rcc_apb1en_tim2)
          "tim2"

tim3 :: GTIM16
tim3 = mkGTIM tim3_periph_base
          (rccEnable rcc_apb1en_tim3)
          (rccDisable rcc_apb1en_tim3)
          "tim3"

tim4 :: GTIM16
tim4 = mkGTIM tim4_periph_base
          (rccEnable rcc_apb1en_tim4)
          (rccDisable rcc_apb1en_tim4)
          "tim4"

tim5 :: GTIM16
tim5 = mkGTIM tim5_periph_base
          (rccEnable rcc_apb1en_tim5)
          (rccDisable rcc_apb1en_tim5)
          "tim5"

-- Both TIM2 and TIM5 are really 32 bit timers, but you can safely make
-- believe they are 16 bit.

tim2_32 :: GTIM32
tim2_32 = mkGTIM tim2_periph_base
            (rccEnable rcc_apb1en_tim2)
            (rccDisable rcc_apb1en_tim2)
            "tim2"

tim5_32 :: GTIM32
tim5_32 = mkGTIM tim5_periph_base
            (rccEnable rcc_apb1en_tim5)
            (rccDisable rcc_apb1en_tim5)
            "tim5"


-- All TIM2345 are in APB1
rccEnable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccEnable f = modifyReg regRCC_APB1ENR $ setBit f
rccDisable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccDisable f = modifyReg regRCC_APB1ENR $ clearBit f

