{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module PX4.Tests.PPMIn (app) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART
import SMACCMPilot.Hardware.PPM.PulseCapture
import SMACCMPilot.Hardware.PPM.Decode

import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified Ivory.BSP.STM32F405.GPIO      as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF   as F405
import qualified Ivory.BSP.STM32F405.ATIM18    as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let console = BSP.testplatform_uart (px4platform_testplatform px4platform)
  (_i,uarto) <- uartTower tocc (BSP.testUARTPeriph console)
                          (BSP.testUARTPins   console)
                          115200
                          (Proxy :: Proxy 128)

  pulse_capt <- channel
  -- XXX FIXME: MOVE THESE PARAMS INTO THE PX4Platform CODE
  pulseCaptureTower F405.tim1 F405.pinA10 F405.gpio_af_tim1 F405.TIM1_CC (fst pulse_capt)
  output <- channel
  ppmDecodeTower (snd pulse_capt) (fst output)

  monitor "ppmsender" $ do
    ppmSender (snd output) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

ppmSender :: ChanOutput (Array 8 (Stored Uint16))
               -> ChanInput  (Stored Uint8)
               -> Monitor e ()
ppmSender samples ostream = do
  (buf :: Ref Global (Array 16 (Stored Uint8))) <- state "ppm_ser_buf"
  handler samples "sample" $ do
    e <- emitter ostream (2*16 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 80 -- 'P' for ppm. lowercase p is position.
