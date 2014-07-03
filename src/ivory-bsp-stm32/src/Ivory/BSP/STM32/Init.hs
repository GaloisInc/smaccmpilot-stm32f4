{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32.Init
  ( stm32InitModule
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.HW.Module (hw_moduledef)

import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.Flash
import Ivory.BSP.STM32.Peripheral.PWR
import Ivory.BSP.STM32.Peripheral.RCC

stm32InitModule :: (PlatformClock p) => Proxy p -> Module
stm32InitModule platform = package "stm32_ivory_init" $ do
  inclHeader "stm32_init.h"
  sourceDep  "stm32_init.h"
  sourceDep  "stm32_init.c"
  incl (reset_handler platform)
  hw_moduledef
  private $ do
    incl (init_clocks platform)
    incl init_relocate
    incl init_libc
    incl main_proc

init_relocate :: Def('[]:->())
init_relocate = externProc "init_relocate"

init_libc :: Def('[]:->())
init_libc = externProc "init_libc"

main_proc :: Def('[]:->())
main_proc = externProc "main"

reset_handler :: (PlatformClock p) => Proxy p -> Def('[]:->())
reset_handler platform = proc (exceptionHandlerName Reset) $ body $ do
  call_ init_relocate
  call_ (init_clocks platform)
  call_ init_libc
  call_ main_proc

init_clocks :: (PlatformClock p) => Proxy p -> Def('[]:->())
init_clocks platform = proc "init_clocks" $ body $ do
  comment ("platformClockConfig: " ++ (show cc)      ++ "\n" ++
           "sysclk: "  ++ (show (clockSysClkHz cc))  ++ "\n" ++
           "hclk:   "  ++ (show (clockHClkHz cc))    ++ "\n" ++
           "pclk1:  "  ++ (show (clockPClk1Hz cc))   ++ "\n" ++
           "pclk2:  "  ++ (show (clockPClk2Hz cc)))

  -- RCC clock config to default reset state
  modifyReg (rcc_reg_cr rcc) $ setBit rcc_cr_hsi_on
  modifyReg (rcc_reg_cfgr rcc) $ do
    setField rcc_cfgr_mco2     rcc_mcox_sysclk
    setField rcc_cfgr_mco2_pre rcc_mcoxpre_none
    setField rcc_cfgr_mco1_pre rcc_mcoxpre_none
    clearBit rcc_cfgr_i2ssrc
    setField rcc_cfgr_mco1     rcc_mcox_sysclk
    setField rcc_cfgr_rtcpre   (fromRep 0)
    setField rcc_cfgr_ppre2    rcc_pprex_none
    setField rcc_cfgr_ppre1    rcc_pprex_none
    setField rcc_cfgr_hpre     rcc_hpre_none
    setField rcc_cfgr_sws      rcc_sysclk_hsi

  -- Reset HSEOn, CSSOn, PLLOn bits
  modifyReg (rcc_reg_cr rcc) $ do
    clearBit rcc_cr_hse_on
    clearBit rcc_cr_css_on
    clearBit rcc_cr_pll_on

  -- Reset PLLCFGR register
  modifyReg (rcc_reg_pllcfgr rcc) $ do
    setField rcc_pllcfgr_pllq   (fromRep 2)
    clearBit rcc_pllcfgr_pllsrc -- use HSI
    setField rcc_pllcfgr_pllp   rcc_pllp_div2
    setField rcc_pllcfgr_plln   (fromRep 192)
    setField rcc_pllcfgr_pllm   (fromRep 16)

  -- Reset HSEBYP bit
  modifyReg (rcc_reg_cr rcc) $ clearBit rcc_cr_hse_byp

  -- Disable all interrupts
  modifyReg (rcc_reg_cir rcc) $ do
    clearBit rcc_cir_plli2s_rdyie
    clearBit rcc_cir_pll_rdyie
    clearBit rcc_cir_hse_rdyie
    clearBit rcc_cir_hsi_rdyie
    clearBit rcc_cir_lse_rdyie
    clearBit rcc_cir_lsi_rdyie
  case clockconfig_source cc of
    Internal -> return ()
    External _ -> do
      -- Enable HSE
      modifyReg (rcc_reg_cr rcc) $ setBit rcc_cr_hse_on

      -- Spin for a little bit waiting for RCC->CR HSERDY bit to be high
      hserdy <- local (ival false)
      arrayMap $ \(_ :: Ix 1024) -> do
        cr <- getReg (rcc_reg_cr rcc)
        when (bitToBool (cr #. rcc_cr_hse_rdy)) $ do
          store hserdy true
          breakOut


      success <- deref hserdy
      when success $ do
        -- Set PLL to use external clock:
        modifyReg (rcc_reg_pllcfgr rcc) $ do
          setBit rcc_pllcfgr_pllsrc -- use HSE

      -- Handle exception case when HSERDY fails.
      unless success $ do
        comment "waiting for HSERDY failed: check your hardware for a fault"
        comment "XXX handle this exception case with a breakpoint or reconfigure pll values for hsi"
        forever $ return ()

  -- Select regulator voltage output scale 1 mode, sys freq 168mhz
  modifyReg (rcc_reg_apb1enr rcc) $ setBit rcc_apb1en_pwr
  modifyReg (pwr_reg_cr pwr) $ setBit pwr_cr_vos

  -- Select bus clock dividers
  modifyReg (rcc_reg_cfgr rcc) $ do
    setField rcc_cfgr_hpre  hpre_divider
    setField rcc_cfgr_ppre1 ppre1_divider
    setField rcc_cfgr_ppre2 ppre2_divider

  -- Configure main PLL:
  modifyReg (rcc_reg_pllcfgr rcc) $ do
    setField rcc_pllcfgr_pllm m
    setField rcc_pllcfgr_plln n
    setField rcc_pllcfgr_pllp p
    setField rcc_pllcfgr_pllq q

  -- Enable main PLL:
  modifyReg (rcc_reg_cr rcc) $ setBit rcc_cr_pll_on
  -- Spin until RCC->CR PLLRDY bit is high
  forever $ do
    cr <- getReg (rcc_reg_cr rcc)
    when (bitToBool (cr #. rcc_cr_pll_rdy)) $ breakOut

  -- Configure flash prefetch, instruction cache, data cache, wait state 5
  modifyReg (flash_reg_acr flash) $ do
    setBit flash_acr_ic_en
    setBit flash_acr_dc_en
    setField flash_acr_latency (fromRep 5)

  -- Select main PLL as system clock source
  modifyReg (rcc_reg_cfgr rcc) $ do
    setField rcc_cfgr_sw rcc_sysclk_pll

  -- Spin until main PLL is ready:
  forever $ do
    cfgr <- getReg (rcc_reg_cfgr rcc)
    when ((cfgr #. rcc_cfgr_sws) ==? rcc_sysclk_pll) $ breakOut

  where
  cc = if clockPLL48ClkHz (platformClockConfig platform) == 48 * 1000 * 1000
          then platformClockConfig platform
          else error "paltformClockConfig invalid: 48MHz peripheral clock is wrong speed"
  mm = pll_m (clockconfig_pll cc)
  m = if mm > 1 && mm < 64
         then fromRep (fromIntegral mm)
         else error "platformClockConfig pll_m not in valid range"
  nn = pll_n (clockconfig_pll cc)
  n = if nn > 191 && nn < 433
         then fromRep (fromIntegral nn)
         else error "platformClockConfig pll_n not in valid range"
  p = case pll_p (clockconfig_pll cc) of
        2 -> rcc_pllp_div2
        4 -> rcc_pllp_div4
        6 -> rcc_pllp_div6
        8 -> rcc_pllp_div8
        _ -> error "platformClockConfig pll_p not in valid range"
  qq = pll_q (clockconfig_pll cc)
  q = if qq > 1 && qq < 16
         then fromRep (fromIntegral qq)
         else error "platformClockConfig pll_q not in valid range"
  hpre_divider = case clockconfig_hclk_divider cc of
    1   -> rcc_hpre_none
    2   -> rcc_hpre_div2
    4   -> rcc_hpre_div4
    8   -> rcc_hpre_div8
    16  -> rcc_hpre_div16
    64  -> rcc_hpre_div64
    128 -> rcc_hpre_div128
    256 -> rcc_hpre_div256
    512 -> rcc_hpre_div512
    _   -> error "platfomClockConfig hclk divider not in valid range"

  ppre1_divider = case clockconfig_pclk1_divider cc of
    1  -> rcc_pprex_none
    2  -> rcc_pprex_div2
    4  -> rcc_pprex_div4
    8  -> rcc_pprex_div8
    16 -> rcc_pprex_div16
    _  -> error "platformClockConfig pclk1 divider not in valid range"

  ppre2_divider = case clockconfig_pclk2_divider cc of
    1  -> rcc_pprex_none
    2  -> rcc_pprex_div2
    4  -> rcc_pprex_div4
    8  -> rcc_pprex_div8
    16 -> rcc_pprex_div16
    _  -> error "platformClockConfig pclk2 divider not in valid range"

