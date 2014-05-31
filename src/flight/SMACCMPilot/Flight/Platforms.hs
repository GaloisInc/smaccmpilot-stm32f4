{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SMACCMPilot.Flight.Platforms where

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.BoardHSE

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

stm32SignalableInstance ''PX4FMU17_IOAR ''F405.Interrupt
stm32SignalableInstance ''PX4FMU17_Bare ''F405.Interrupt

instance BoardHSE PX4FMU17_IOAR where
  hseFreqHz _ = 24000000

instance BoardHSE PX4FMU17_Bare where
  hseFreqHz _ = 24000000

