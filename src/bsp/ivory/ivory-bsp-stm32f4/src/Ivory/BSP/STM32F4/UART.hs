{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
--
-- SPI.hs --- UART driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.UART where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.MemoryMap

----------------------------------------------------------------------
-- UART Registers

[bitdata|
 bitdata UART_SR :: Bits 16 = uart_sr
   { _                :: Bits 6
   , uart_sr_cts      :: Bit
   , uart_sr_lbd      :: Bit
   , uart_sr_txe      :: Bit
   , uart_sr_tc       :: Bit
   , uart_sr_rxne     :: Bit
   , uart_sr_idle     :: Bit
   , uart_sr_orne     :: Bit
   , uart_sr_nf       :: Bit
   , uart_sr_fe       :: Bit
   , uart_sr_pe       :: Bit
   }

 bitdata UART_DR :: Bits 16 = uart_dr
   { _                  :: Bits 8
   , uart_dr_data       :: Bits 8
   }

 bitdata UART_BRR :: Bits 16 = uart_brr
   { uart_brr_div_mantissa :: Bits 12
   , uart_brr_div_fraction :: Bits 4
   }

 bitdata UART_CR1 :: Bits 16 = uart_cr1
   { uart_cr1_over8    :: Bit
   , _                 :: Bit
   , uart_cr1_ue       :: Bit
   , uart_cr1_m        :: Bit
   , uart_cr1_wake     :: Bit
   , uart_cr1_pce      :: Bit
   , uart_cr1_ps       :: Bit
   , uart_cr1_peie     :: Bit
   , uart_cr1_txeie    :: Bit
   , uart_cr1_tcie     :: Bit
   , uart_cr1_rxneie   :: Bit
   , uart_cr1_idleie   :: Bit
   , uart_cr1_te       :: Bit
   , uart_cr1_re       :: Bit
   , uart_cr1_rwu      :: Bit
   , uart_cr1_sbk      :: Bit
   }

 bitdata UART_CR2 :: Bits 16 = uart_cr2
   { _                 :: Bit
   , uart_cr2_linen    :: Bit
   , uart_cr2_stop     :: Bits 2
   , uart_cr2_clken    :: Bit
   , uart_cr2_cpol     :: Bit
   , uart_cr2_cpha     :: Bit
   , uart_cr2_lbcl     :: Bit
   , _                 :: Bit
   , uart_cr2_lbdie    :: Bit
   , uart_cr2_lbdl     :: Bit
   , _                 :: Bit
   , uart_cr2_add      :: Bits 4
   }

 bitdata UART_CR3 :: Bits 16 = uart_cr3
   { _                 :: Bits 4
   , uart_cr3_onebit   :: Bit
   , uart_cr3_ctsie    :: Bit
   , uart_cr3_ctse     :: Bit
   , uart_cr3_rtse     :: Bit
   , uart_cr3_dmat     :: Bit
   , uart_cr3_dmar     :: Bit
   , uart_cr3_scen     :: Bit
   , uart_cr3_nack     :: Bit
   , uart_cr3_hdsel    :: Bit
   , uart_cr3_irlp     :: Bit
   , uart_cr3_iren     :: Bit
   , uart_cr3_eie      :: Bit
   }

 bitdata UART_GTPR :: Bits 16 = uart_gtpr
   { uart_gtpr_gt      :: Bits 8
   , uart_gtpr_psc     :: Bits 8
   }

|]

----------------------------------------------------------------------
-- UART Data Structures

data UART = UART
  { uartRegSR   :: BitDataReg UART_SR
  , uartRegDR   :: BitDataReg UART_DR
  , uartRegBRR  :: BitDataReg UART_BRR
  , uartRegCR1  :: BitDataReg UART_CR1
  , uartRegCR2  :: BitDataReg UART_CR2
  , uartRegCR3  :: BitDataReg UART_CR3
  , uartRegGTPR :: BitDataReg UART_GTPR
  }

-- | Construct a "UART" given a base address for the registers.
mkUART :: Integer -> UART
mkUART base = UART
  { uartRegSR   = mkBitDataReg (base + 0x00)
  , uartRegDR   = mkBitDataReg (base + 0x04)
  , uartRegBRR  = mkBitDataReg (base + 0x08)
  , uartRegCR1  = mkBitDataReg (base + 0x0C)
  , uartRegCR2  = mkBitDataReg (base + 0x10)
  , uartRegCR3  = mkBitDataReg (base + 0x14)
  , uartRegGTPR = mkBitDataReg (base + 0x18)
  }

uart1, uart2, uart3, uart4, uart5, uart6 :: UART
uart1 = mkUART (apb2periph_base + 0x1000)
uart2 = mkUART (apb1periph_base + 0x4400)
uart3 = mkUART (apb1periph_base + 0x4800)
uart4 = mkUART (apb1periph_base + 0x4C00)
uart5 = mkUART (apb1periph_base + 0x5000)
uart6 = mkUART (apb2periph_base + 0x1400)

