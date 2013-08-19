{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW
import Ivory.BitData

import Ivory.Tower.Frontend
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.GPIO

import LEDTower

app :: Tower p ()
app = do
  blinkApp 250  [led1]
  blinkApp 500  [led2]
  blinkApp 1000 [led3]
  blinkApp 2000 [led4]
  where
  led1 = LED pinD12 ActiveHigh
  led2 = LED pinD13 ActiveHigh
  led3 = LED pinD14 ActiveHigh
  led4 = LED pinD15 ActiveHigh

main = compilePlatforms conf [("open407vc", Twr app)]
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]

