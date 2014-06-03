
--
-- MemoryMap.hs -- Memory map (addresses) for STM32
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.MemoryMap where

periph_base :: Integer
periph_base = 0x40000000

apb1_periph_base, apb2_periph_base :: Integer
apb1_periph_base = periph_base
apb2_periph_base = periph_base + 0x00010000

ahb1_periph_base, ahb2_periph_base :: Integer
ahb1_periph_base = periph_base + 0x00020000
ahb2_periph_base = periph_base + 0x10000000

-- APB1 Peripherals
pwr_periph_base :: Integer
pwr_periph_base = apb1_periph_base + 0x7000

-- AHB1 Peripherals
rcc_periph_base :: Integer
rcc_periph_base = ahb1_periph_base + 0x3800
flash_r_periph_base :: Integer
flash_r_periph_base = ahb1_periph_base + 0x3C00

