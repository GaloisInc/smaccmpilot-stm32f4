{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.Compile
  ( compile
  ) where

import Ivory.Opts.CFG (SizeMap(..))
import Ivory.Tower
import Ivory.Tower.Frontend hiding (compile)

import qualified Ivory.Stdlib.SearchDir as Stdlib

import qualified Ivory.HW.SearchDir as HW

import           Ivory.BSP.STM32F4.Signalable
import qualified Ivory.BSP.STM32.SearchDir   as BSP
import qualified Ivory.BSP.STM32F4.RCC       as BSP

import SMACCMPilot.Flight.Platforms
import SMACCMPilot.Flight.Motors.Platforms
import SMACCMPilot.Flight.Sensors.Platforms

compile :: (forall p . ( STM32F4Signal p, BSP.BoardHSE p
                       , MotorOutput p, SensorOrientation p)
        => Tower p ())
        -> [String]
        -> IO ()
compile app = compilePlatforms' conf ps
  where
  sp   = searchPathConf [Stdlib.searchDir, HW.searchDir, BSP.searchDir]
  conf = sp { bc_sizemap = Just sizeMap }
  ps   = [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
         ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
         ]

-- Sizes in terms bytes.
sizeMap :: SizeMap
sizeMap = SizeMap
  { stackElemMap = const 1
  , retSize = 4
  }
