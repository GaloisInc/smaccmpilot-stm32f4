{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import Ivory.BSP.STM32F405.ClockConfig

import qualified SMACCMPilot.Hardware.PX4IOAR as IOAR
import qualified SMACCMPilot.Hardware.PX4FMU17 as Bare

class RawMotorControl p where
  rawMotorControl :: ChannelSink (Array 4 (Stored IFloat)) -> Tower p ()

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt

instance RawMotorControl PX4FMU17_IOAR where
  rawMotorControl = IOAR.motorControlTower cpystack

instance PlatformClock PX4FMU17_IOAR where
  platformClockConfig _ = f405ExtXtalMHz 24

instance RawMotorControl PX4FMU17_Bare where
  rawMotorControl = Bare.motorControlTower cpystack

instance PlatformClock PX4FMU17_Bare where
  platformClockConfig _ = f405ExtXtalMHz 24

cpystack :: ConstRef s (Array 4 (Stored IFloat))
         -> Ivory (AllocEffects cs)
              (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
cpystack v = do
  l <- local (iarray [])
  arrayMap $ \i -> deref (v ! i) >>= store (l ! i)
  return (constRef l)

motorPlatforms :: (forall p . (RawMotorControl p, PlatformClock p
                    , STM32Signal p, InterruptType p ~ F405.Interrupt)
                    => Tower p ())
               -> [(String, Twr)]
motorPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ]
