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

motorControlTower :: (SingI n, IvoryArea a, IvoryZero a)
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
