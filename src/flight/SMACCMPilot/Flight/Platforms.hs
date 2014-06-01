{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SMACCMPilot.Flight.Platforms where

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import           Ivory.BSP.STM32F405.ClockConfig
import           Ivory.BSP.STM32.Signalable
import           Ivory.BSP.STM32.PlatformClock

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt

instance PlatformClock PX4FMU17_IOAR where
  platformClockConfig _ = f405ExtXtalMHz 24

instance PlatformClock PX4FMU17_Bare where
  platformClockConfig _ = f405ExtXtalMHz 24

