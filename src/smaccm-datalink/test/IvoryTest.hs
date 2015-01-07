{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language

import Ivory.Tower
import Tower.Config
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.UART

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms


main :: IO ()
main = towerCompile p (app id)
  where
  p topts = do
    cfg <- getConfig topts px4PlatformParser
    return $ stm32FreeRTOS px4platform_stm32config cfg

app :: (e -> PX4Platform F405.Interrupt)
    -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  (_i,_o) <- uartTower tocc (console px4platform) 115200 (Proxy :: Proxy 128)
  return () -- XXX use io here.
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4
  console = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform

