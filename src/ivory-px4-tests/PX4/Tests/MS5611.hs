{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MS5611 (ms5611Sender, ms5611ctl, app) where

import Ivory.Language
import Ivory.Serialize
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MS5611

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let ms5611 = px4platform_ms5611_device px4platform
  (req, res, _ready) <- i2cTower tocc
                         (ms5611device_periph ms5611)
                         (ms5611device_sda ms5611)
                         (ms5611device_scl ms5611)
  measurements <- ms5611ctl req res (ms5611device_addr ms5611)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti, uarto) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 128)
  monitor "ms5611sender" $ do
    ms5611Sender measurements uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

ms5611Sender :: ChanOutput (Struct "ms5611_measurement")
             -> ChanInput (Stored Uint8)
             -> Monitor e ()
ms5611Sender meas out = do
  (buf :: Ref Global (Array 18 (Stored Uint8))) <- state "ms5611_ser_buf"
  handler meas "measurement" $ do
    e <- emitter out (2*18 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 98 -- 'b' for barometer
