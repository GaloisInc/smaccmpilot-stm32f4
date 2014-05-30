{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- GetFreq.hs --- Run-time frequency information from RCC driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.RCC.GetFreq where

import Ivory.Language
import Ivory.BitData

import Ivory.BSP.STM32.BoardHSE

eqBits :: (BitData a) => a -> a -> IBool
eqBits l r = (toBits l) ==? (toBits r)

hsiFreqHz :: Integer
hsiFreqHz = 16 * 1000 * 1000

hsiFreq :: Uint32
hsiFreq = fromIntegral hsiFreqHz

-- always true for stm32f405. won't be true for other chips!
sysClkHz :: Integer
sysClkHz = 168 * 1000 * 1000

-- XXX these no longer depend on BoardHSE.
-- we should just make these Integer constants and then

getFreqSysClk :: (GetAlloc eff ~ Scope s, BoardHSE p)
              => Proxy p -> Ivory eff Uint32
getFreqSysClk _ = return (fromIntegral sysClkHz)

-- the three dividers here - 1, 4, 2 - are dependent on the init.hs setup
getFreqHClk :: (GetAlloc eff ~ Scope s, BoardHSE p)
            => Proxy p -> Ivory eff Uint32
getFreqHClk _ = return (fromIntegral (div sysClkHz 1))

getFreqPClk1 :: (GetAlloc eff ~ Scope s, BoardHSE p)
             => Proxy p -> Ivory eff Uint32
getFreqPClk1 _ = return (fromIntegral (div sysClkHz 4))

getFreqPClk2 :: (GetAlloc eff ~ Scope s, BoardHSE p)
             => Proxy p -> Ivory eff Uint32
getFreqPClk2 _ = return (fromIntegral (div sysClkHz 2))

data PClk = PClk1 | PClk2

getFreqPClk :: (GetAlloc eff ~ Scope s, BoardHSE p) => Proxy p -> PClk -> Ivory eff Uint32
getFreqPClk p PClk1 = getFreqPClk1 p
getFreqPClk p PClk2 = getFreqPClk2 p

