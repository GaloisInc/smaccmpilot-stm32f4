
module Ivory.BSP.STM32F405.ClockConfig
  ( f405ExtXtalMHz
  ) where

import Ivory.BSP.STM32.ClockConfig

-- Give a clock config for a given external crystal frequency in MHz
f405ExtXtalMHz :: Integer -> ClockConfig
f405ExtXtalMHz xtalfreq = ClockConfig
  { clockconfig_source = External (xtalfreq * 1000 * 1000)
  , clockconfig_pll    = PLLFactor
      { pll_m = xtalfreq
      , pll_n = 336
      , pll_p = 2
      , pll_q = 7
      }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 4
  , clockconfig_pclk2_divider = 2
  }
