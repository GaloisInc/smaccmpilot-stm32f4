{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- SPI register types
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.SPI.RegTypes where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

[bitdata|
 bitdata SPIBaud :: Bits 3
   = spi_baud_div_2   as 0
   | spi_baud_div_4   as 1
   | spi_baud_div_8   as 2
   | spi_baud_div_16  as 3
   | spi_baud_div_32  as 4
   | spi_baud_div_64  as 5
   | spi_baud_div_128 as 6
   | spi_baud_div_256 as 7
|]
