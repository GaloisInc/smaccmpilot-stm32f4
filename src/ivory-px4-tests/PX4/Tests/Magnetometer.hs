{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.Magnetometer (hmc5883lSender, hmc5883lSensorManager, app) where

import Ivory.Language
import Ivory.Serialize

import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.LSM303D

import PX4.Tests.Platforms

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (_uarti,uarto) <- px4ConsoleTower topx4

  case px4platform_mag px4platform of
    Mag_HMC5883L_I2C h -> hmc5883l_i2c_app topx4 h uarto
    Mag_LSM303D_SPI l -> lsm303d_spi_app topx4 l uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

hmc5883l_i2c_app :: (e -> PX4Platform)
                 -> HMC5883L_I2C
                 -> ChanInput (Stored Uint8)
                 -> Tower e ()
hmc5883l_i2c_app topx4 hmc uarto = do
  (req, res, ready) <- i2cTower (px4platform_clockconfig . topx4)
                         (hmc5883l_i2c_periph hmc)
                         (hmc5883l_i2c_sda    hmc)
                         (hmc5883l_i2c_scl    hmc)

  samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  hmc5883lSensorManager req res sensors_ready (fst samples) (hmc5883l_i2c_addr hmc)

  monitor "hmc5883lsender" $ do
    hmc5883lSender (snd samples) uarto


hmc5883lSender :: ChanOutput (Struct "hmc5883l_sample")
               -> ChanInput  (Stored Uint8)
               -> Monitor e ()
hmc5883lSender samples ostream = do
  (buf :: Ref Global (Array 34 (Stored Uint8))) <- state "hmc5883l_ser_buf"
  handler samples "sample" $ do
    e <- emitter ostream (2*34 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 99 -- 'c' for compass

-------

lsm303d_spi_app :: (e -> PX4Platform)
                 -> LSM303D_SPI
                 -> ChanInput (Stored Uint8)
                 -> Tower e ()
lsm303d_spi_app topx4 lsm uarto = do
  (req, res, ready) <- spiTower (px4platform_clockconfig . topx4)
                         [lsm303d_spi_device lsm]
                         (lsm303d_spi_pins lsm)

  samples <- channel

  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  lsm303dSPISensorManager lsm303dDefaultConf req res sensors_ready
                          (fst samples) (SPIDeviceHandle 0)

  monitor "lsm303dSender" $ do
    lsm303dSender (snd samples) uarto

lsm303dSender :: ChanOutput (Struct "lsm303d_sample")
              -> ChanInput  (Stored Uint8)
              -> Monitor e ()
lsm303dSender samples ostream = do
  (buf :: Ref Global (Array 34 (Stored Uint8))) <- state "lsm303d_sample_buf"
  handler samples "sample" $ do
    e <- emitter ostream (2*34 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 108 -- 'l' for lsm303
