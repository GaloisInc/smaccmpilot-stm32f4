{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
--
-- Peripheral.hs --- UART Peripheral Description
-- UART Peripheral type and constructor
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.UART.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.UART.Regs

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.GPIO.AF
import Ivory.BSP.STM32F4.MemoryMap
import Ivory.BSP.STM32F4.Interrupt

data UART = UART
  { uartRegSR      :: BitDataReg UART_SR
  , uartRegDR      :: BitDataReg UART_DR
  , uartRegBRR     :: BitDataReg UART_BRR
  , uartRegCR1     :: BitDataReg UART_CR1
  , uartRegCR2     :: BitDataReg UART_CR2
  , uartRegCR3     :: BitDataReg UART_CR3
  , uartRegGTPR    :: BitDataReg UART_GTPR
  , uartRCCEnable  :: forall eff . Ivory eff ()
  , uartRCCDisable :: forall eff . Ivory eff ()
  , uartPinTx      :: GPIOPin
  , uartPinRx      :: GPIOPin
  , uartPinAF      :: GPIO_AF
  , uartInterrupt  :: Interrupt
  }

instance RCCDevice UART where
  rccDeviceEnable  = uartRCCEnable
  rccDeviceDisable = uartRCCDisable

mkUART :: (BitData a, IvoryIOReg (BitDataRep a))
       => Integer
       -> BitDataReg a
       -> BitDataField a Bit
       -> GPIOPin -- Transmit
       -> GPIOPin -- Receive
       -> GPIO_AF
       -> Interrupt
       -> UART
mkUART base rccreg rccfield tx rx af interrupt = UART
  { uartRegSR      = mkBitDataReg (base + 0x00)
  , uartRegDR      = mkBitDataReg (base + 0x04)
  , uartRegBRR     = mkBitDataReg (base + 0x08)
  , uartRegCR1     = mkBitDataReg (base + 0x0C)
  , uartRegCR2     = mkBitDataReg (base + 0x10)
  , uartRegCR3     = mkBitDataReg (base + 0x14)
  , uartRegGTPR    = mkBitDataReg (base + 0x18)
  , uartRCCEnable  = rccEnable  rccreg rccfield
  , uartRCCDisable = rccDisable rccreg rccfield
  , uartPinTx      = tx
  , uartPinRx      = rx
  , uartPinAF      = af
  , uartInterrupt  = interrupt
  }

uart1, uart2, uart3, uart4, uart5, uart6 :: UART
uart1 = mkUART uart1_periph_base
                regRCC_APB2ENR rcc_apb2en_uart1 pinB6  pinB7  gpio_af_uart1 USART1
uart2 = mkUART uart2_periph_base
                regRCC_APB1ENR rcc_apb1en_uart2 pinA2  pinA3  gpio_af_uart2 USART2
uart3 = mkUART uart3_periph_base
                regRCC_APB1ENR rcc_apb1en_uart3 pinB10 pinB12 gpio_af_uart3 USART3
uart4 = mkUART uart4_periph_base
                regRCC_APB1ENR rcc_apb1en_uart4 pinC10 pinC11 gpio_af_uart4 UART4
uart5 = mkUART uart5_periph_base
                regRCC_APB1ENR rcc_apb1en_uart5 pinC12 pinD2  gpio_af_uart5 UART5
uart6 = mkUART uart6_periph_base
                regRCC_APB2ENR rcc_apb2en_uart6 pinC6  pinC7  gpio_af_uart6 USART6

