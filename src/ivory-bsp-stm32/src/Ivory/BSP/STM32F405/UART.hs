--
-- UART.hs --- UART Driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--


module Ivory.BSP.STM32F405.UART
  ( UART()
  , uart1, uart2, uart3, uart4, uart5, uart6
  , uartInit
  , uartInitISR
  , setBaudRate
--  , uartTower
  ) where

import Ivory.BSP.STM32F405.UART.Peripheral
-- import Ivory.BSP.STM32F405.UART.Tower

