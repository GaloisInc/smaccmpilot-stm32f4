
--
-- MemoryMap.hs -- Memory map (addresses) for STM32F4
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.MemoryMap where

flash_base, ccmdataram_base, sram1_base, sram2_base :: Integer
flash_base      = 0x08000000
ccmdataram_base = 0x10000000
sram1_base      = 0x20000000
sram2_base      = 0x2001C000

periph_base :: Integer
periph_base = 0x40000000

bkpsram_base :: Integer
bkpsram_base = 0x40024000

apb1periph_base, apb2periph_base :: Integer
apb1periph_base = periph_base
apb2periph_base = periph_base + 0x00010000

ahb1periph_base, ahb2periph_base :: Integer
ahb1periph_base = periph_base + 0x00020000
ahb2periph_base = periph_base + 0x10000000

