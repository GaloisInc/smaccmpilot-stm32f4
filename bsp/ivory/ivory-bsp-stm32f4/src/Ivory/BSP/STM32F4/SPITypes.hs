{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
--
-- SPITypes.hs --- SPI driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.SPITypes where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

[bitdata|
 bitvalue SPIBaud :: Bits 3
   = SPIBaudDiv2   as 0
   | SPIBaudDiv4   as 1
   | SPIBaudDiv8   as 2
   | SPIBaudDiv16  as 3
   | SPIBaudDiv32  as 4
   | SPIBaudDiv64  as 5
   | SPIBaudDiv128 as 6
   | SPIBaudDiv256 as 7
|]
