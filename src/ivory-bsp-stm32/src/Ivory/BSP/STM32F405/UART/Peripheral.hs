{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
--
-- Peripheral.hs --- UART Peripheral Description
-- UART Peripheral type and constructor
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.UART.Peripheral where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.PlatformClock

import Ivory.BSP.STM32F405.UART.Types
import Ivory.BSP.STM32F405.UART.Regs

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.GPIO.AF
import Ivory.BSP.STM32F405.MemoryMap
import qualified Ivory.BSP.STM32F405.Interrupt as F405

data UART i = UART
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
  , uartInterrupt  :: i
  , uartPClk       :: PClk
  , uartName       :: String
  }

mkUART :: (BitData a, IvoryIOReg (BitDataRep a))
       => Integer
       -> BitDataReg a
       -> BitDataField a Bit
       -> GPIOPin -- Transmit
       -> GPIOPin -- Receive
       -> GPIO_AF
       -> i
       -> PClk
       -> String
       -> UART i
mkUART base rccreg rccfield tx rx af interrupt pclk n = UART
  { uartRegSR      = reg 0x00 "sr"
  , uartRegDR      = reg 0x04 "dr"
  , uartRegBRR     = reg 0x08 "brr"
  , uartRegCR1     = reg 0x0C "cr1"
  , uartRegCR2     = reg 0x10 "cr2"
  , uartRegCR3     = reg 0x14 "cr3"
  , uartRegGTPR    = reg 0x18 "gtpr"
  , uartRCCEnable  = rccEnable  rccreg rccfield
  , uartRCCDisable = rccDisable rccreg rccfield
  , uartPinTx      = tx
  , uartPinRx      = rx
  , uartPinAF      = af
  , uartInterrupt  = interrupt
  , uartPClk       = pclk
  , uartName       = n
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

uart1, uart2, uart3, uart4, uart5, uart6 :: UART F405.Interrupt
uart1 = mkUART uart1_periph_base
                regRCC_APB2ENR rcc_apb2en_uart1 pinB6  pinB7
                gpio_af_uart1 F405.USART1 PClk2 "uart1"
uart2 = mkUART uart2_periph_base
                regRCC_APB1ENR rcc_apb1en_uart2 pinA2  pinA3
                gpio_af_uart2 F405.USART2 PClk1 "uart2"
uart3 = mkUART uart3_periph_base
                regRCC_APB1ENR rcc_apb1en_uart3 pinB10 pinB12
                gpio_af_uart3 F405.USART3 PClk1 "uart3"
uart4 = mkUART uart4_periph_base
                regRCC_APB1ENR rcc_apb1en_uart4 pinC10 pinC11
                gpio_af_uart4 F405.UART4 PClk1 "uart4"
uart5 = mkUART uart5_periph_base
                regRCC_APB1ENR rcc_apb1en_uart5 pinC12 pinD2
                gpio_af_uart5 F405.UART5 PClk1 "uart5"
uart6 = mkUART uart6_periph_base
                regRCC_APB2ENR rcc_apb2en_uart6 pinC6  pinC7
                gpio_af_uart6 F405.USART6 PClk2 "uart6"

-- | Initialize GPIO pins for a UART.
initPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
initPin p af = do
  pinEnable        p
  pinSetSpeed      p gpio_speed_50mhz
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetPUPD       p gpio_pupd_pullup
  pinSetAF         p af
  pinSetMode       p gpio_mode_af

-- | Set the BRR register of a UART given a baud rate.
setBaudRate :: (GetAlloc eff ~ Scope s, PlatformClock p) => UART i -> Proxy p -> Uint32 -> Ivory eff ()
setBaudRate uart platform baud = do
  pclk    <- assign =<< getFreqPClk platform (uartPClk uart)
  cr1     <- getReg (uartRegCR1 uart)
  isOver8 <- assign (bitToBool (cr1 #. uart_cr1_over8))
  ipart   <- assign ((25 * pclk) `iDiv` (baud * (isOver8 ? (2, 4))))
  mask    <- assign ((ipart `iDiv` 100) `iShiftL` 4)
  fpart   <- assign (ipart - (100 * (mask `iShiftR` 4)))
  brr     <- assign (mask .|
                     (isOver8 ?
                      ( (((fpart * 8)  + 50) `iDiv` 100) .& 0x07
                      , (((fpart * 16) + 50) `iDiv` 100) .& 0x0f)))
  setReg (uartRegBRR uart) $ do
    setField uart_brr_div (fromRep (lbits brr))

-- | Configure the stop bits for a UART.
setStopBits :: UART i -> UART_StopBits -> Ivory eff ()
setStopBits uart x =
  modifyReg (uartRegCR2 uart) $
    setField uart_cr2_stop x

-- | Configure the word length for a UART.
setWordLen :: UART i -> UART_WordLen -> Ivory eff ()
setWordLen uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_m x

-- | Configure the parity setting for a UART.
--
-- TODO: Not writing functions to set the parity mode unless we
-- actually need to use that.  Only "no parity" mode is currently
-- supported.
setParity :: UART i -> IBool -> Ivory eff ()
setParity uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_pce (boolToBit x)

-- | Initialize a UART device given a baud rate.
uartInit :: (GetAlloc eff ~ Scope s, PlatformClock p)
         => UART i -> Proxy p -> Uint32 -> Ivory eff ()
uartInit uart platform baud = do
  -- Enable the peripheral clock and set up GPIOs.
  uartRCCEnable uart
  initPin (uartPinTx uart) (uartPinAF uart)
  initPin (uartPinRx uart) (uartPinAF uart)

  -- Initialize the baud rate and other settings.
  setBaudRate uart platform baud
  setStopBits uart uart_stop_bits_1_0
  setWordLen  uart uart_word_len_8
  setParity   uart false

  -- Enable the UART, transmitter, and receiver.
  modifyReg (uartRegCR1 uart) $ do
    setBit uart_cr1_ue
    setBit uart_cr1_te
    setBit uart_cr1_re

uartInitISR :: (STM32Interrupt i, GetAlloc eff ~ Scope s)
            => UART i -> Ivory eff ()
uartInitISR uart = do
  interrupt_set_to_syscall_priority inter
  interrupt_enable                  inter
  setRXNEIE uart true
  where
  inter = uartInterrupt uart

-- | Set the UART data register.
setDR :: UART i -> Uint8 -> Ivory eff ()
setDR uart b =
  setReg (uartRegDR uart) $
    setField uart_dr_data (fromRep b)

-- | Read the UART data register.
readDR :: UART i -> Ivory eff Uint8
readDR uart = do
  dr <- getReg (uartRegDR uart)
  return (toRep (dr #. uart_dr_data))

-- | Enable or disable the "TXE" interrupt.
setTXEIE :: UART i -> IBool -> Ivory eff ()
setTXEIE uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_txeie (boolToBit x)

-- | See whether "TXE" interrupt is enabled.
getTXEIE :: UART i -> Ivory eff IBool
getTXEIE uart = do
  cr1 <- getReg (uartRegCR1 uart)
  return $ bitToBool (cr1 #. uart_cr1_txeie)

-- | Enable or disable the "RXNE" interrupt.
setRXNEIE :: UART i -> IBool -> Ivory eff ()
setRXNEIE uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_rxneie (boolToBit x)

