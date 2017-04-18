{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Hardware.Tests.Magnetometer
  ( app
  ) where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.HMC5883L (hmc5883lSensorManager)
import SMACCMPilot.Hardware.LSM303D

import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Hardware.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (uarto, _uarti, mon) <- px4ConsoleTower topx4
  monitor "console_uart" mon

  case px4platform_mag px4platform of
    Mag_HMC5883L_I2C h mag_cal ->
      hmc5883l_i2c_app topx4 mag_cal h uarto
    Mag_LSM303D_SPI l mag_cal accel_cal ->
      lsm303d_spi_app topx4 mag_cal accel_cal l uarto

  serializeTowerDeps

hmc5883l_i2c_app :: (e -> PX4Platform)
                 -> MagCal
                 -> HMC5883L_I2C
                 -> BackpressureTransmit ConsoleBuffer ('Stored IBool)
                 -> Tower e ()
hmc5883l_i2c_app topx4 mag_cal hmc uarto = do
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                         (hmc5883l_i2c_periph hmc)
                         (hmc5883l_i2c_pins   hmc)

  samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  hmc5883lSensorManager mag_cal
    req sensors_ready (fst samples) (hmc5883l_i2c_addr hmc)

  monitor "hmc5883lsender" $ do
    magSender (snd samples) uarto


-------

lsm303d_spi_app :: (e -> PX4Platform)
                -> MagCal
                -> AccelCal
                -> LSM303D_SPI
                -> BackpressureTransmit ConsoleBuffer ('Stored IBool)
                -> Tower e ()
lsm303d_spi_app topx4 mag_cal accel_cal lsm uarto = do
  (req, ready) <- spiTower (px4platform_clockconfig . topx4)
                         [lsm303d_spi_device lsm]
                         (lsm303d_spi_pins lsm)

  m_samples <- channel
  a_samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  lsm303dSPISensorManager lsm303dDefaultConf req sensors_ready
                          (fst m_samples) mag_cal
                          (fst a_samples) accel_cal
                          (SPIDeviceHandle 0)

  (magTask, magReq) <- task "mag"
  (accTask, accReq) <- task "acc"

  monitor "lsm303dSender" $ do
    magSender (snd m_samples) magReq
    accelSender (snd a_samples) accReq

  schedule "uart" [magTask, accTask] systemInit uarto
