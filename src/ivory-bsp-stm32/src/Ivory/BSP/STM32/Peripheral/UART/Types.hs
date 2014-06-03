{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Types.hs --- UART type definitions.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.UART.Types where

import Ivory.Language

[ivory|
 bitdata UART_WordLen :: Bits 1
   = uart_word_len_8 as 0
   | uart_word_len_9 as 1

 bitdata UART_StopBits :: Bits 2
   = uart_stop_bits_1_0 as 0
   | uart_stop_bits_0_5 as 1
   | uart_stop_bits_2_0 as 2
   | uart_stop_bits_1_5 as 3

|]
