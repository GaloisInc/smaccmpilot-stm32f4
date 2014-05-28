--
-- GTIM2345.hs --- General Purpose Timer (TIM2 to TIM5) driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.GTIM2345
  ( GTIM(..)
  , GTIM32
  , GTIM16
  , tim2
  , tim2_32
  , tim3
  , tim4
  , tim5
  , tim5_32
  , module Ivory.BSP.STM32F405.GTIM2345.Regs
  ) where

import Ivory.BSP.STM32F405.GTIM2345.Peripheral
import Ivory.BSP.STM32F405.GTIM2345.Regs

