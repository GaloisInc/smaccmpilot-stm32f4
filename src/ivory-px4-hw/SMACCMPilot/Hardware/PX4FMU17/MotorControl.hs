{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4FMU17.MotorControl
  ( motorControlTower
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32F4.GTIM2345
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.GPIO.AF
import Ivory.BSP.STM32F4.RCC

motorControlTower :: (SingI n, IvoryArea a, IvoryZero a, BoardHSE p)
             => (forall s cs . ConstRef s a
                  -> Ivory (AllocEffects cs)
                       (ConstRef (Stack cs) (Array 4 (Stored IFloat))))
             -> ChannelSink n a
             -> Tower p ()
motorControlTower decode motorChan = do
  task "px4fmu17_pwm" $ do
    istream <- withChannelEvent   motorChan  "motor_istream"
    taskInit $ do
      return ()
    onEvent istream $ \throttle -> do
      return ()

hw_init :: Ivory eff ()
hw_init = do
  tim_init tim2
  mapM_ (pwm_out_pin gpio_af_tim2) [pinA0, pinA1, pinA2, pinA3]

pwm_out_pin :: GPIO_AF -> GPIOPin -> Ivory eff ()
pwm_out_pin af p = do
  pinEnable        p
  pinSetMode       p gpio_mode_af
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetAF         p af

tim_init :: GTIM16 -> Ivory eff ()
tim_init t = do
  gtimRCCEnable t
  -- XXXXX

