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

import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.ClockConfig
import qualified Ivory.BSP.STM32F405.Interrupt as F405

import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC
data PX4FMU24      = PX4FMU24

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt
stm32SignalableInstance ''Open407VC     ''F405.Interrupt
stm32SignalableInstance ''PX4FMU24      ''F405.Interrupt

class GPSUart p where
  consoleUart :: Proxy p -> UART F405.Interrupt
  gpsUart     :: Proxy p -> UART F405.Interrupt

instance PlatformClock PX4FMU17_IOAR where
  platformClockConfig _ = f405ExtXtalMHz 24
instance GPSUart PX4FMU17_IOAR where
  consoleUart _ = uart1
  gpsUart _ = uart6

instance PlatformClock PX4FMU17_Bare where
  platformClockConfig _ = f405ExtXtalMHz 24
instance GPSUart PX4FMU17_Bare where
  consoleUart _ = uart1
  gpsUart _ = uart6

instance PlatformClock Open407VC where
  platformClockConfig _ = f405ExtXtalMHz 8
instance GPSUart Open407VC where
  consoleUart _ = uart1
  gpsUart _ = uart2

instance PlatformClock PX4FMU24 where
  platformClockConfig _ = f405ExtXtalMHz 24
instance GPSUart PX4FMU24 where
  consoleUart _ = uart1
  gpsUart _ = uart3

gpsPlatforms :: (forall p . (GPSUart p, PlatformClock p, STM32Signal p, InterruptType p ~ F405.Interrupt)
                  => Tower p ())
             -> [(String, Twr)]
gpsPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",      Twr (app :: Tower PX4FMU24 ()))
    ]
