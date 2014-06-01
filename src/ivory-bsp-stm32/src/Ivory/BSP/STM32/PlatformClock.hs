
module Ivory.BSP.STM32.PlatformClock where

import Ivory.Language
import Ivory.BSP.STM32.ClockConfig

class PlatformClock p where
  platformClockConfig :: Proxy p -> ClockConfig
