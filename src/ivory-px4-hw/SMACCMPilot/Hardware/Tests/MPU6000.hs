{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Hardware.Tests.MPU6000
  ( mpu6000SensorManager
  , app
  ) where

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.MPU6000

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  g_sample <- channel
  a_sample <- channel
  px4platform <- fmap topx4 getEnv
  let mpu6000  = px4platform_mpu6000 px4platform
  (req, ready) <- spiTower (px4platform_clockconfig . topx4)
                                [mpu6000_spi_device mpu6000]
                                (mpu6000_spi_pins mpu6000)

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  mpu6000SensorManager req sensors_ready (fst g_sample) (fst a_sample) (SPIDeviceHandle 0)

  (uarto, _uarti) <- px4ConsoleTower topx4
  half_g <- rateDivider 2 (snd g_sample)
  half_a <- rateDivider 2 (snd a_sample)

  uartTasks <- sequence
    [ do
        (t, chan) <- task name
        monitor name $ f chan
        return t
    | (name, f) <-
      [ ("gyro", gyroSender half_g)
      , ("accel", accelSender half_a)
      ]
    ]
  schedule uartTasks systemInit uarto

  serializeTowerDeps

