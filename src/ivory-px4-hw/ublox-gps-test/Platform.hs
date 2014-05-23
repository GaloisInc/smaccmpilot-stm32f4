{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.Signalable

f24MHz :: Integer
f24MHz = 24000000
f8MHz :: Integer
f8MHz = 8000000

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare
data Open407VC     = Open407VC
data PX4FMU24      = PX4FMU24

stm32f4SignalableInstance ''PX4FMU17_IOAR
stm32f4SignalableInstance ''PX4FMU17_Bare
stm32f4SignalableInstance ''Open407VC
stm32f4SignalableInstance ''PX4FMU24

class GPSUart p where
  consoleUart :: Proxy p -> UART
  gpsUart     :: Proxy p -> UART

instance BoardHSE PX4FMU17_IOAR where
  hseFreqHz _ = f24MHz
instance GPSUart PX4FMU17_IOAR where
  consoleUart _ = uart1
  gpsUart _ = uart6

instance BoardHSE PX4FMU17_Bare where
  hseFreqHz _ = f24MHz
instance GPSUart PX4FMU17_Bare where
  consoleUart _ = uart1
  gpsUart _ = uart6

instance BoardHSE Open407VC where
  hseFreqHz _ = f8MHz
instance GPSUart Open407VC where
  consoleUart _ = uart1
  gpsUart _ = uart2

instance BoardHSE PX4FMU24 where
  hseFreqHz _ = f24MHz
instance GPSUart PX4FMU24 where
  consoleUart _ = uart1
  gpsUart _ = uart3

gpsPlatforms :: (forall p . (GPSUart p, BoardHSE p, STM32F4Signal p)
                  => Tower p ())
             -> [(String, Twr)]
gpsPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ,("open407vc",     Twr (app :: Tower Open407VC ()))
    ,("px4fmu24",      Twr (app :: Tower PX4FMU24 ()))
    ]
