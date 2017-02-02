{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4FMU17.MotorControl
  ( motorControlTower
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.Tower

import Ivory.BSP.STM32.Peripheral.GPIOF4
-- Safe for now - fmu 1.7 always uses f405
import Ivory.BSP.STM32F405.GTIM2345
import Ivory.BSP.STM32F405.GPIO.AF
import Ivory.BSP.STM32F405.GPIO

import Ivory.BSP.STM32.ClockConfig

-- In microseconds:
minPWM, maxPWM :: Uint16
minPWM = 1100
maxPWM = 1900

motorControlTower :: (e -> ClockConfig)
                  -> ChanOutput ('Array 4 ('Stored IFloat))
                  -> Tower e ()
motorControlTower tocc ostream = do
  cc <- fmap tocc getEnv
  monitor "px4fmu17_pwm" $ do
    monitorModuleDef $ hw_moduledef
    handler systemInit "init" $ callback $ const $ hw_init cc
    handler ostream "pwm_output" $ do
      callback $ \throttle -> noReturn $ do
        pwm_output throttle

hw_init :: (GetAlloc eff ~ 'Scope cs)
        => ClockConfig -> Ivory eff ()
hw_init clockConfig = do
  tim_init clockConfig tim2
  mapM_ (pwm_out_pin gpio_af_tim2) [pinA0, pinA1, pinA2, pinA3]

pwm_out_pin :: GPIO_AF -> GPIOPin -> Ivory eff ()
pwm_out_pin af p = do
  pinEnable        p
  pinSetMode       p gpio_mode_af
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetAF         p af

tim_init :: (GetAlloc eff ~ 'Scope cs)
         => ClockConfig -> GTIM16 -> Ivory eff ()
tim_init clockConfig gtim = do
  gtimRCCEnable gtim
  -- Set the timer prescaler for 1MHz operation:
  -- TIM2345 timer input is 2*PCLK1
  let fpclk1 = clockPClk1Hz clockConfig
  timFreq <- assign (fromIntegral (2*fpclk1))
  let finput = 1000000 :: Uint32
  tdivider <- assign $ (timFreq `iDiv` finput) - 1
  tdividerI16 <- assign $ castWith 0 tdivider
  setReg (gtimRegPSC gtim) $ setField gtim_16_data (fromRep tdividerI16)
  -- Set the auto reload at 50Hz:
  let freload = 50
      tarr = castWith 0 $ finput `iDiv` freload
  setReg (gtimRegARR gtim) $ setField gtim_16_data (fromRep tarr)
  -- Generate an update event to reload the counter and all registers:
  setReg (gtimRegEGR gtim) $ setBit gtim_egr_ug
  -- Initialize all channels:
  initpwm <- local (iarray [ ival minPWM, ival minPWM, ival minPWM, ival minPWM ])
  pwm_set gtim initpwm
  -- Turn on the output compare registers:
  setReg (gtimRegCCMR1_OCM gtim) $ do
    setField gtim_ccmr1_ocm_cc1s ccs_mode_out
    setField gtim_ccmr1_ocm_cc2s ccs_mode_out
    setField gtim_ccmr1_ocm_oc1m ccmr_mode_pwm1
    setField gtim_ccmr1_ocm_oc2m ccmr_mode_pwm1
    setBit gtim_ccmr1_ocm_oc1pe
    setBit gtim_ccmr1_ocm_oc2pe
  setReg (gtimRegCCMR2_OCM gtim) $ do
    setField gtim_ccmr2_ocm_cc3s ccs_mode_out
    setField gtim_ccmr2_ocm_cc4s ccs_mode_out
    setField gtim_ccmr2_ocm_oc3m ccmr_mode_pwm1
    setField gtim_ccmr2_ocm_oc4m ccmr_mode_pwm1
    setBit gtim_ccmr2_ocm_oc3pe
    setBit gtim_ccmr2_ocm_oc4pe
  -- Enable output compare to trigger pin output:
  setReg (gtimRegCCER gtim) $ do
    setBit gtim_ccer_cc1e
    setBit gtim_ccer_cc2e
    setBit gtim_ccer_cc3e
    setBit gtim_ccer_cc4e
  -- Enable counter and set auto reload to use buffering:
  setReg (gtimRegCR1 gtim) $ do
    setBit gtim_cr1_cen
    setBit gtim_cr1_arpe

pwm_set :: GTIM16 -> Ref s ('Array 4 ('Stored Uint16)) -> Ivory eff ()
pwm_set gtim chs = do
  set_ccr (gtimRegCCR1 gtim) (chs ! 1)
  set_ccr (gtimRegCCR2 gtim) (chs ! 2)
  set_ccr (gtimRegCCR3 gtim) (chs ! 0)
  set_ccr (gtimRegCCR4 gtim) (chs ! 3)
  where
  set_ccr reg chan = do
    c <- deref chan
    setReg reg (setField gtim_16_data (fromRep c))

pwm_scale :: (GetAlloc eff ~ 'Scope cs)
          => ConstRef s ('Array 4 ('Stored IFloat))
          -> Ivory eff (Ref ('Stack cs) ('Array 4 ('Stored Uint16)))
pwm_scale throttle = do
  o <- local (iarray [])
  arrayMap $ \i -> do
    t <- deref (throttle ! i)
    store (o ! i) ((t <? idle)?(minPWM, scale t))
  return o
  where
  scale :: IFloat -> Uint16
  scale t = castWith 0 $ (t * range) + offs
  range :: IFloat
  range = safeCast (maxPWM - minPWM)
  offs :: IFloat
  offs = safeCast minPWM
  idle = 0.07

pwm_output :: (GetAlloc eff ~ 'Scope cs)
           => ConstRef s ('Array 4 ('Stored IFloat))
           -> Ivory eff ()
pwm_output throttle = do
  scaled <- pwm_scale throttle
  pwm_set tim2 scaled
