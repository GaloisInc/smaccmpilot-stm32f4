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

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.LSM303D

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (uarto, _uarti) <- px4ConsoleTower topx4

  case px4platform_mag px4platform of
    Mag_HMC5883L_I2C h -> hmc5883l_i2c_app topx4 h uarto
    Mag_LSM303D_SPI l -> lsm303d_spi_app topx4 l uarto

  serializeTowerDeps

hmc5883l_i2c_app :: (e -> PX4Platform)
                 -> HMC5883L_I2C
                 -> BackpressureTransmit ConsoleBuffer (Stored IBool)
                 -> Tower e ()
hmc5883l_i2c_app topx4 hmc uarto = do
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                         (hmc5883l_i2c_periph hmc)
                         (hmc5883l_i2c_pins   hmc)

  samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  hmc5883lSensorManager req sensors_ready (fst samples) (hmc5883l_i2c_addr hmc)

  monitor "hmc5883lsender" $ do
    magSender (snd samples) uarto


-------

lsm303d_spi_app :: (e -> PX4Platform)
                 -> LSM303D_SPI
                 -> BackpressureTransmit ConsoleBuffer (Stored IBool)
                 -> Tower e ()
lsm303d_spi_app topx4 lsm uarto = do
  (req, ready) <- spiTower (px4platform_clockconfig . topx4)
                         [lsm303d_spi_device lsm]
                         (lsm303d_spi_pins lsm)

  m_samples <- channel
  a_samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  lsm303dSPISensorManager lsm303dDefaultConf req sensors_ready
                          (fst m_samples) (fst a_samples)
                          (SPIDeviceHandle 0)

  monitor "lsm303dSender" $ do
    magSender (snd m_samples) uarto
    accelSender (snd a_samples) uarto

