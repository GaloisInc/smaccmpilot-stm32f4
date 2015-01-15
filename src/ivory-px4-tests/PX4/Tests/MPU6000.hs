{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MPU6000
  ( mpu6000SensorManager
  , mpu6000Sender
  , app
  ) where

import Ivory.Language
import Ivory.Serialize
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MPU6000

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  sample <- channel
  px4platform <- fmap topx4 getEnv
  let mpu6000 = px4platform_mpu6000_device px4platform
  (req, res, ready) <- spiTower tocc [mpu6000]
  mpu6000SensorManager req res ready (fst sample) (SPIDeviceHandle 0)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti, uarto) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 256)
  monitor "mpu6000sender" $ do
    mpu6000Sender (snd sample) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  towerModule  mpu6000TypesModule
  towerDepends mpu6000TypesModule
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

mpu6000Sender :: ChanOutput (Struct "mpu6000_sample")
              -> ChanInput (Stored Uint8)
              -> Monitor e ()
mpu6000Sender meas out = do
  (buf :: Ref Global (Array 38 (Stored Uint8))) <- state "mpu6000_ser_buf"
  handler meas "measurement" $ do
    e <- emitter out (38*2 + 3)
    callback $ \s -> noReturn $ do
      packInto_ buf 0 $ mpack s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 103 -- 'g' for gyro
