
module Ivory.BSP.STM32F405.SPI
  ( spi1
  , spi2
  , spi3
  ) where


import Ivory.BSP.STM32.Peripheral.SPI

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.GPIO.AF
import Ivory.BSP.STM32F405.MemoryMap
import qualified Ivory.BSP.STM32F405.Interrupt as F405

spi1, spi2, spi3 :: SPIPeriph F405.Interrupt
spi1 = mkSPIPeriph spi1_periph_base rccenable rccdisable
          pinA7  pinA6  pinA5  gpio_af_spi1 F405.SPI1 PClk2 "spi1"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit rcc_apb2en_spi1
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_spi1

spi2 = mkSPIPeriph spi2_periph_base rccenable rccdisable
          pinC3  pinC2  pinB10 gpio_af_spi2 F405.SPI2 PClk1 "spi2"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit rcc_apb1en_spi2
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_spi2

spi3 = mkSPIPeriph spi3_periph_base rccenable rccdisable
          pinC12 pinC11 pinC10 gpio_af_spi3 F405.SPI3 PClk1 "spi3"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit rcc_apb1en_spi3
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_spi3
