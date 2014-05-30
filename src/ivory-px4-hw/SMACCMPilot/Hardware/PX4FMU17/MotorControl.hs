{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4FMU17.MotorControl
  ( motorControlTower
  ) where

import Ivory.Language
import Ivory.HW
import Ivory.HW.Module
import Ivory.BitData
import Ivory.Tower

import Ivory.BSP.STM32F405.GTIM2345
import Ivory.BSP.STM32F405.GTIM2345.RegTypes
import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.GPIO.AF
import Ivory.BSP.STM32F405.RCC

-- In microseconds:
minPWM, maxPWM :: Uint16
minPWM = 1100
maxPWM = 1900

motorControlTower :: forall a p
                   . (IvoryArea a, IvoryZero a, BoardHSE p)
                  => (forall s cs . ConstRef s a
                       -> Ivory (AllocEffects cs)
                            (ConstRef (Stack cs) (Array 4 (Stored IFloat))))
                  -> ChannelSink a
                  -> Tower p ()
motorControlTower decode motorChan = do
  task "px4fmu17_pwm" $ do
    istream <- withChannelEvent   motorChan  "motor_istream"
    taskModuleDef $ hw_moduledef
    taskInit $ hw_init (Proxy :: Proxy p)
    handle istream "encThrottle" $ \encThrottle -> noReturn $ do
      throttle <- decode encThrottle
      pwm_output throttle

hw_init :: (BoardHSE p, GetAlloc eff ~ Scope cs)
        => Proxy p -> Ivory eff ()
hw_init platform = do
  tim_init platform tim2
  mapM_ (pwm_out_pin gpio_af_tim2) [pinA0, pinA1, pinA2, pinA3]
--  pwm_arm tim2

pwm_out_pin :: GPIO_AF -> GPIOPin -> Ivory eff ()
pwm_out_pin af p = do
  pinEnable        p
  pinSetMode       p gpio_mode_af
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetAF         p af

tim_init :: (BoardHSE p, GetAlloc eff ~ Scope cs)
         => Proxy p -> GTIM16 -> Ivory eff ()
tim_init platform gtim = do
  gtimRCCEnable gtim
  -- Set the timer prescaler for 1MHz operation:
  -- TIM2345 timer input is 2*PCLK1
  timFreq <- (2*) `fmap` getFreqPClk1 platform
  let finput = 1000000
  tdivider <- assign $ (timFreq `iDiv` finput) - 1
  tdividerInt <- assign $ castWith 0 tdivider
  setReg (gtimRegPSC gtim) $ setField gtim_16_data (fromRep tdividerInt)
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


-- pwm_arm :: GTIM16 -> Ivory eff ()
-- pwm_arm gtim = do
--   -- Generate an update event to reload the counter and all registers:
--   setReg (gtimRegEGR gtim) $ setBit gtim_egr_ug
--   -- Enable counter and set auto reload to use buffering:
--   setReg (gtimRegCR1 gtim) $ do
--     setBit gtim_cr1_cen
--     setBit gtim_cr1_arpe

pwm_set :: GTIM16 -> Ref s (Array 4 (Stored Uint16)) -> Ivory eff ()
pwm_set gtim chs = do
  set_ccr (gtimRegCCR1 gtim) (chs ! 1)
  set_ccr (gtimRegCCR2 gtim) (chs ! 2)
  set_ccr (gtimRegCCR3 gtim) (chs ! 0)
  set_ccr (gtimRegCCR4 gtim) (chs ! 3)
  where
  set_ccr reg chan = do
    c <- deref chan
    setReg reg (setField gtim_16_data (fromRep c))

pwm_scale :: (GetAlloc eff ~ Scope cs)
          => ConstRef s (Array 4 (Stored IFloat))
          -> Ivory eff (Ref (Stack cs) (Array 4 (Stored Uint16)))
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

pwm_output :: (GetAlloc eff ~ Scope cs)
           => ConstRef s (Array 4 (Stored IFloat))
           -> Ivory eff ()
pwm_output throttle = do
  scaled <- pwm_scale throttle
  pwm_set tim2 scaled

