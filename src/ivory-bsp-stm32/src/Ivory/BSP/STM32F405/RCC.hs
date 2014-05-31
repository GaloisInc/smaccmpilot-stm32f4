--
-- RCC.hs --- RCC (Reset and Clock Control) peripheral driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.RCC
  ( module Ivory.BSP.STM32F405.RCC.Regs
  , module Ivory.BSP.STM32F405.RCC.RegTypes
  , rccEnable
  , rccDisable
  -- * system clock frequency
  , PClk(..)

  , getFreqSysClk
  , getFreqHClk
  , getFreqPClk1
  , getFreqPClk2
  , getFreqPClk
  ) where

import Ivory.BSP.STM32F405.RCC.Class
import Ivory.BSP.STM32F405.RCC.Regs
import Ivory.BSP.STM32F405.RCC.RegTypes
import Ivory.BSP.STM32F405.RCC.GetFreq
