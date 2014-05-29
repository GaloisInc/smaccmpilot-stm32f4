{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- NVIC registers.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.Interrupt.Regs where

import Data.Bits ((.&.), shiftR)
import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32F4.MemoryMap
import Ivory.BSP.STM32F4.Interrupt.Types

----------------------------------------------------------------------
-- NVIC Registers

[ivory|
  bitdata NVIC_ISER :: Bits 32 = nvic_iser
    { nvic_iser_setena :: BitArray 32 Bit }

  bitdata NVIC_ICER :: Bits 32 = nvic_icer
    { nvic_icer_clrena :: BitArray 32 Bit }

  bitdata NVIC_ISPR :: Bits 32 = nvic_ispr
    { nvic_ispr_setpend :: BitArray 32 Bit }

  bitdata NVIC_ICPR :: Bits 32 = nvic_icpr
    { nvic_icpr_clrpend :: BitArray 32 Bit }

  -- Rather than model the IPR registers as 32-bit registers
  -- containing 4 8-bit values, we create 8-bit registers directly,
  -- since these registers are byte addressable.  This saves us a
  -- read/write/modify cycle when setting an interrupt priority.

  -- bitdata NVIC_IPR :: Bits 32 = nvic_ipr
  --   { nvic_ipr_ip :: BitArray 4 (Bits 8) }
|]

-- | Base address for the "ISER" (interrupt set enable) registers.
nvic_ISER_base :: Integer
nvic_ISER_base = nvic_base + 0x000

-- | Return the "n"th "ISER" register.
nvic_ISER :: Int -> BitDataReg NVIC_ISER
nvic_ISER n = mkBitDataRegNamed addr ("nvic_iser" ++ show n)
  where addr = nvic_ISER_base + (fromIntegral n) * 4

-- | Base address for the "ICER" (interrupt clear enable) registers.
nvic_ICER_base :: Integer
nvic_ICER_base = nvic_base + 0x080

-- | Return the "n"th "ICER" register.
nvic_ICER :: Int -> BitDataReg NVIC_ICER
nvic_ICER n = mkBitDataRegNamed addr ("nvic_icer" ++ show n)
  where addr = nvic_ICER_base + (fromIntegral n) * 4

-- | Base address for the "ISPR" (interrupt set pending) registers.
nvic_ISPR_base :: Integer
nvic_ISPR_base = nvic_base + 0x100

-- | Return the "n"th "ISPR" register.
nvic_ISPR :: Int -> BitDataReg NVIC_ISPR
nvic_ISPR n = mkBitDataRegNamed addr ("nvic_ispr" ++ show n)
  where addr = nvic_ISPR_base + (fromIntegral n) * 4

-- | Base address for the "ICPR" (interrupt clear pending) registers.
nvic_ICPR_base :: Integer
nvic_ICPR_base = nvic_base + 0x180

-- | Return the "n"th "ICPR" register.
nvic_ICPR :: Int -> BitDataReg NVIC_ICPR
nvic_ICPR n = mkBitDataRegNamed addr ("nvic_icpr" ++ show n)
  where addr = nvic_ICPR_base + (fromIntegral n) * 4

-- | Base address for the "IPR" (interrupt priority) registers.
nvic_IPR_base :: Integer
nvic_IPR_base = nvic_base + 0x300

-- | Return the "n"th "IPR" register.
nvic_IPR :: Int -> Reg Uint8
nvic_IPR n = mkReg addr
  where addr = nvic_IPR_base + (fromIntegral n)

-- | Return the NVIC_ISER register and bit number for an interrupt.
nvic_ISER_int :: Interrupt -> (BitDataReg NVIC_ISER, Int)
nvic_ISER_int i = (reg, bitN)
  where
    irqN = fromIntegral (interruptIRQn i) :: Int
    reg  = nvic_ISER (irqN `shiftR` 5)
    bitN = irqN .&. 0x1F

-- | Return the NVIC_ICER register and bit number for an interrupt.
nvic_ICER_int :: Interrupt -> (BitDataReg NVIC_ICER, Int)
nvic_ICER_int i = (reg, bitN)
  where
    irqN = fromIntegral (interruptIRQn i) :: Int
    reg  = nvic_ICER (irqN `shiftR` 5)
    bitN = irqN .&. 0x1F

-- | Return the NVIC_ISPR register and bit number for an interrupt.
nvic_ISPR_int :: Interrupt -> (BitDataReg NVIC_ISPR, Int)
nvic_ISPR_int i = (reg, bitN)
  where
    irqN = fromIntegral (interruptIRQn i) :: Int
    reg  = nvic_ISPR (irqN `shiftR` 5)
    bitN = irqN .&. 0x1F

-- | Return the NVIC_ICPR register and bit number for an interrupt.
nvic_ICPR_int :: Interrupt -> (BitDataReg NVIC_ICPR, Int)
nvic_ICPR_int i = (reg, bitN)
  where
    irqN = fromIntegral (interruptIRQn i) :: Int
    reg  = nvic_ICPR (irqN `shiftR` 5)
    bitN = irqN .&. 0x1F

-- | Return the 8-bit priority register for an interrupt.
nvic_IPR_int :: Interrupt -> Reg Uint8
nvic_IPR_int i = nvic_IPR irqN
  where
    irqN = fromIntegral (interruptIRQn i) :: Int
    -- reg  = nvic_IPR (irqN `div` 4)
