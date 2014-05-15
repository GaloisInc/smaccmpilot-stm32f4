
--
-- MemoryMap.hs -- Memory map (addresses) for ARMv7M
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.ARMv7M.MemoryMap where

-- System Control Space

system_control_base :: Integer
system_control_base = 0xE000E000

systick_base :: Integer
systick_base = system_control_base + 0x010

nvic_base :: Integer
nvic_base = system_control_base + 0x100

mpu_base :: Integer
mpu_base = system_control_base + 0xD90

debug_base :: Integer
debug_base = system_control_base + 0xDF0

