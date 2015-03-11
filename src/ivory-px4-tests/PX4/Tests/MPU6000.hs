{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MPU6000
  ( mpu6000SensorManager
  , app
  ) where

import Ivory.Tower

import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.MPU6000

import PX4.Tests.Platforms
import PX4.Tests.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  g_sample <- channel
  a_sample <- channel
  px4platform <- fmap topx4 getEnv
  let mpu6000  = px4platform_mpu6000 px4platform
  (req, res, ready) <- spiTower (px4platform_clockconfig . topx4)
                                [mpu6000_spi_device mpu6000]
                                (mpu6000_spi_pins mpu6000)

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  mpu6000SensorManager req res sensors_ready (fst g_sample) (fst a_sample) (SPIDeviceHandle 0)

  (_uarti, uarto) <- px4ConsoleTower topx4
  half_g <- halfRate (snd g_sample)
  half_a <- halfRate (snd a_sample)
  monitor "mpu6000sender" $ do
    gyroSender half_g uarto
    accelSender half_a uarto
  serializeTowerDeps

