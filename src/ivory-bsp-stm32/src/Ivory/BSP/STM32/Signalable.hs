{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.BSP.STM32.Signalable
  ( module Ivory.BSP.STM32.Signalable.Class
  , module Ivory.BSP.STM32.Signalable.TH
  ) where

import Ivory.Tower.Types.Signalable
import Ivory.BSP.STM32.Signalable.Class
import Ivory.BSP.STM32.Signalable.TH
