{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

--
-- GetFreq.hs --- Run-time frequency information from RCC driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC.GetFreq where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.RCC.RegTypes
import Ivory.BSP.STM32F4.RCC.Regs

-- Not sure what class constraints needed to write eqBits

eqBits :: (BitData a) => a -> a -> IBool
eqBits l r = (toBits l) ==? (toBits r)

hsiFreq = 16000000 -- from stm32f4xx.h
hseFreq = 24000000 -- Actually depends on preprocessor value!

getFreqSysClk :: (eff `AllocsIn` s) => Ivory eff Uint32
getFreqSysClk = do
  cfg <- getReg regRCC_CFGR
  clk <- local (ival 0)
  let sws = getBitDataField rcc_cfgr_sws cfg
  ifte (eqBits sws rcc_sysclk_hsi)
    (store clk hsiFreq)
    (ifte (eqBits sws rcc_sysclk_hse)
          (store clk hseFreq)
          (ifte (eqBits sws rcc_sysclk_pll)
                (pllSysClk >>= (store clk))
                ({- DEFAULT CASE -} store clk hsiFreq)))
  deref clk

pllSysClk :: (eff `AllocsIn` s) => Ivory eff Uint32
pllSysClk = undefined

getFreqHClk :: Ivory eff Uint32
getFreqHClk = undefined

getFreqPClk1 :: Ivory eff Uint32
getFreqPClk1 = undefined

getFreqPClk2 :: Ivory eff Uint32
getFreqPClk2 = undefined

