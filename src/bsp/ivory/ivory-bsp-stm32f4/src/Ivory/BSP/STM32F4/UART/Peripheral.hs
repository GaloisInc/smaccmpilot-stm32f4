{-# LANGUAGE TypeFamilies #-}
--
-- Regs.hs --- UART Peripheral Description
-- Public types and API to the UART driver
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
import Ivory.BSP.STM32F4.MemoryMap

data UART = UART
  { uartRegSR          :: BitDataReg UART_SR
  , uartRegDR          :: BitDataReg UART_DR
  , uartRegBRR         :: BitDataReg UART_BRR
  , uartRegCR1         :: BitDataReg UART_CR1
  , uartRegCR2         :: BitDataReg UART_CR2
  , uartRegCR3         :: BitDataReg UART_CR3
  , uartRegGTPR        :: BitDataReg UART_GTPR
  , uartRCCEnableField :: BitDataField (RCCEnableReg UART) Bit
  }

-- XXX this is broken at the moment - I'm going to ask James for advice
-- resolving it.
-- the uart1 and uart6 enable bits are in RCC_APB2ENR.
instance RCCDevice UART where
  type RCCEnableReg UART = RCC_APB1ENR
  rccDeviceEnableReg _ = regRCC_APB1ENR
  rccDeviceEnableField = uartRCCEnableField

-- | Construct a "UART" given a base address for the registers.
mkUART :: Integer -> BitDataField (RCCEnableReg UART) Bit  -> UART
mkUART base rccenable = UART
  { uartRegSR   = mkBitDataReg (base + 0x00)
  , uartRegDR   = mkBitDataReg (base + 0x04)
  , uartRegBRR  = mkBitDataReg (base + 0x08)
  , uartRegCR1  = mkBitDataReg (base + 0x0C)
  , uartRegCR2  = mkBitDataReg (base + 0x10)
  , uartRegCR3  = mkBitDataReg (base + 0x14)
  , uartRegGTPR = mkBitDataReg (base + 0x18)
  , uartRCCEnableField = rccenable
  }

uart1, uart2, uart3, uart4, uart5, uart6 :: UART
uart1 = mkUART uart1_periph_base undefined          -- XXX problematic
uart2 = mkUART uart2_periph_base rcc_apb1en_uart2
uart3 = mkUART uart3_periph_base rcc_apb1en_uart3
uart4 = mkUART uart4_periph_base rcc_apb1en_uart4
uart5 = mkUART uart5_periph_base rcc_apb1en_uart5
uart6 = mkUART uart6_periph_base undefined          -- XXX problematic

