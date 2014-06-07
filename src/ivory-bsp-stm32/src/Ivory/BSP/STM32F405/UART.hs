--
-- UART.hs --- UART Driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--


module Ivory.BSP.STM32F405.UART
  ( uart1, uart2, uart3, uart4, uart5, uart6
  ) where

import Ivory.BSP.STM32.Peripheral.UART

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.GPIO.AF
import Ivory.BSP.STM32F405.MemoryMap
import qualified Ivory.BSP.STM32F405.Interrupt as F405

uart1, uart2, uart3, uart4, uart5, uart6 :: UART F405.Interrupt
uart1 = mkUART uart1_periph_base
                rccenable rccdisable
                pinB6  pinB7
                gpio_af_uart1 F405.USART1 PClk2 "uart1"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit   rcc_apb2en_uart1
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_uart1

uart2 = mkUART uart2_periph_base
                rccenable rccdisable
                pinA2  pinA3
                gpio_af_uart2 F405.USART2 PClk1 "uart2"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart2
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart2

uart3 = mkUART uart3_periph_base
                rccenable rccdisable
                pinB10 pinB12
                gpio_af_uart3 F405.USART3 PClk1 "uart3"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart3
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart3

uart4 = mkUART uart4_periph_base
                rccenable rccdisable
                pinC10 pinC11
                gpio_af_uart4 F405.UART4 PClk1 "uart4"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart4
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart4

uart5 = mkUART uart5_periph_base
                rccenable rccdisable
                pinC12 pinD2
                gpio_af_uart5 F405.UART5 PClk1 "uart5"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart5
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart5

uart6 = mkUART uart6_periph_base
                rccenable rccdisable
                pinC6  pinC7
                gpio_af_uart6 F405.USART6 PClk2 "uart6"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit   rcc_apb2en_uart6
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_uart6

