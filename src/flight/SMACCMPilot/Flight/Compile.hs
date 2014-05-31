{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Flight.Compile
  ( compile
  ) where

import Ivory.Opts.CFG (SizeMap(..))
import Ivory.Tower
import Ivory.Tower.Frontend hiding (compile)

import qualified Ivory.Stdlib.SearchDir as Stdlib

import qualified Ivory.HW.SearchDir as HW

import qualified Ivory.BSP.STM32.SearchDir     as BSP
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import           Ivory.BSP.STM32.Signalable
import           Ivory.BSP.STM32.BoardHSE

import SMACCMPilot.Flight.Platforms
import SMACCMPilot.Flight.Motors.Platforms
import SMACCMPilot.Flight.Sensors.Platforms

compile :: (forall p . ( STM32Signal F405.Interrupt p, BoardHSE p
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
