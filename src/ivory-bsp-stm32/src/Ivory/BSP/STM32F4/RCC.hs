--
-- RCC.hs --- RCC (Reset and Clock Control) peripheral driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC (
    RCCDevice(..), rccEnable, rccDisable
  , module Ivory.BSP.STM32F4.RCC.Regs
  , module Ivory.BSP.STM32F4.RCC.RegTypes

  -- * system clock frequency
  , PClk(..)

  , BoardHSE(..) -- XXX eventaully extract these two
  , hseFreq

  , getFreqSysClk
  , getFreqHClk
  , getFreqPClk1
  , getFreqPClk2
  , getFreqPClk
) where

import Ivory.BSP.STM32.BoardHSE
import Ivory.BSP.STM32F4.RCC.Class
import Ivory.BSP.STM32F4.RCC.Regs
import Ivory.BSP.STM32F4.RCC.RegTypes
import Ivory.BSP.STM32F4.RCC.GetFreq
