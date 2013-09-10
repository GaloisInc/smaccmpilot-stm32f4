
module SMACCMPilot.Flight.Platforms where

import Ivory.BSP.STM32F4.RCC

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

instance BoardHSE PX4FMU17_IOAR where
  hseFreq _ = 24000000

instance BoardHSE PX4FMU17_Bare where
  hseFreq _ = 24000000

