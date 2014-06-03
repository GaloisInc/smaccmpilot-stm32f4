{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- NVIC.hs --- ARMv7M NVIC registers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.ARMv7M.SystemControl.NVIC where

import Data.Bits ((.&.), shiftR)
import Ivory.Language

import Ivory.HW

import Ivory.BSP.ARMv7M.MemoryMap
import Ivory.BSP.ARMv7M.Exception

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
nvic_ISER_nth :: Integer -> BitDataReg NVIC_ISER
nvic_ISER_nth n = mkBitDataRegNamed addr ("nvic_iser" ++ show n)
  where addr = nvic_ISER_base + n * 4

-- | Return the NVIC_ISER register and bit number for an interrupt.
nvic_ISER :: IRQn -> (BitDataReg NVIC_ISER, Int)
nvic_ISER i = (reg, bitN)
  where
    irqN = fromIntegral (unIRQn i) :: Int
    reg  = nvic_ISER_nth (fromIntegral (irqN `shiftR` 5))
    bitN = irqN .&. 0x1F

-- | Base address for the "ICER" (interrupt clear enable) registers.
nvic_ICER_base :: Integer
nvic_ICER_base = nvic_base + 0x080

-- | Return the "n"th "ICER" register.
nvic_ICER_nth :: Integer -> BitDataReg NVIC_ICER
nvic_ICER_nth n = mkBitDataRegNamed addr ("nvic_icer" ++ show n)
  where addr = nvic_ICER_base + n * 4

-- | Return the NVIC_ICER register and bit number for an interrupt.
nvic_ICER :: IRQn -> (BitDataReg NVIC_ICER, Int)
nvic_ICER i = (reg, bitN)
  where
    irqN = fromIntegral (unIRQn i) :: Int
    reg  = nvic_ICER_nth (fromIntegral (irqN `shiftR` 5))
    bitN = irqN .&. 0x1F

-- | Base address for the "ISPR" (interrupt set pending) registers.
nvic_ISPR_base :: Integer
nvic_ISPR_base = nvic_base + 0x100

-- | Return the "n"th "ISPR" register.
nvic_ISPR_nth :: Integer -> BitDataReg NVIC_ISPR
nvic_ISPR_nth n = mkBitDataRegNamed addr ("nvic_ispr" ++ show n)
  where addr = nvic_ISPR_base + n * 4

-- | Return the NVIC_ISPR register and bit number for an interrupt.
nvic_ISPR :: IRQn -> (BitDataReg NVIC_ISPR, Int)
nvic_ISPR i = (reg, bitN)
  where
    irqN = fromIntegral (unIRQn i) :: Int
    reg  = nvic_ISPR_nth (fromIntegral (irqN `shiftR` 5))
    bitN = irqN .&. 0x1F

-- | Base address for the "ICPR" (interrupt clear pending) registers.
nvic_ICPR_base :: Integer
nvic_ICPR_base = nvic_base + 0x180

-- | Return the NVIC_ICPR register and bit number for an interrupt.
nvic_ICPR :: IRQn -> (BitDataReg NVIC_ICPR, Int)
nvic_ICPR i = (reg, bitN)
  where
    irqN = fromIntegral (unIRQn i) :: Int
    reg  = nvic_ICPR_nth (fromIntegral (irqN `shiftR` 5))
    bitN = irqN .&. 0x1F

-- | Return the "n"th "ICPR" register.
nvic_ICPR_nth :: Integer -> BitDataReg NVIC_ICPR
nvic_ICPR_nth n = mkBitDataRegNamed addr ("nvic_icpr" ++ show n)
  where
  addr = nvic_ICPR_base + n * 4

-- | Base address for the "IPR" (interrupt priority) registers.
nvic_IPR_base :: Integer
nvic_IPR_base = nvic_base + 0x300

-- | Return the "n"th "IPR" register.
nvic_IPR_nth :: Integer -> Reg Uint8
nvic_IPR_nth i = mkReg addr
  where addr = nvic_IPR_base + i

-- | Return the 8-bit priority register for an interrupt.
nvic_IPR :: IRQn -> Reg Uint8
nvic_IPR i = nvic_IPR_nth irqN
  where
    irqN = unIRQn i

