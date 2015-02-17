{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module PX4.Tests.PPMIn (app) where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.RingBuffer
import SMACCMPilot.Hardware.PPM.PulseCapture

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified Ivory.BSP.STM32F405.GPIO      as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF   as F405
import qualified Ivory.BSP.STM32F405.ATIM18    as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt)
    -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  c <- channel
  let console = BSP.testplatform_uart (px4platform_testplatform px4platform)
  (i,o) <- uartTower tocc (BSP.testUARTPeriph console)
                          (BSP.testUARTPins   console)
                          115200
                          (Proxy :: Proxy 128)

  pulseCaptureTower F405.tim1 F405.pinA10 F405.gpio_af_tim1 F405.TIM1_CC (fst c)
  monitor "buffer" $ do
    (rb :: RingBuffer 128 (Struct "pulse_capture")) <- monitorRingBuffer "loopback"
    handler (snd c) "pulses" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4
