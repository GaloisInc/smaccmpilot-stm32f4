{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module SMACCMPilot.Flight.Platforms where

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

stm32f4SignalableInstance ''PX4FMU17_IOAR
stm32f4SignalableInstance ''PX4FMU17_Bare

instance BoardHSE PX4FMU17_IOAR where
  hseFreqHz _ = 24000000

instance BoardHSE PX4FMU17_Bare where
  hseFreqHz _ = 24000000

