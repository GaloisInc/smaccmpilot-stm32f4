{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Hardware.Tests.Baro (ms5611I2CSensorManager, app) where

import Ivory.Language

import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.MS5611

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  measurements <- channel

  case px4platform_baro px4platform of
    Baro_MS5611_I2C m -> ms5611_i2c_app topx4 m (fst measurements)
    Baro_MS5611_SPI m -> ms5611_spi_app topx4 m (fst measurements)


  (uarto, _uarti) <- px4ConsoleTower topx4
  monitor "ms5611sender" $ do
    baroSender (snd measurements) uarto

  serializeTowerDeps

ms5611_i2c_app :: (e -> PX4Platform)
               -> MS5611_I2C
               -> ChanInput (Struct "barometer_sample")
               -> Tower e ()
ms5611_i2c_app topx4 ms5611 meas = do
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                         (ms5611_i2c_periph ms5611)
                         (ms5611_i2c_pins ms5611)
  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  ms5611I2CSensorManager req sensors_ready meas (ms5611_i2c_addr ms5611)

ms5611_spi_app :: (e -> PX4Platform)
               -> MS5611_SPI
               -> ChanInput (Struct "barometer_sample")
               -> Tower e ()
ms5611_spi_app topx4 ms5611 meas = do
  (req, ready) <- spiTower (px4platform_clockconfig . topx4)
                         [ms5611_spi_device ms5611]
                         (ms5611_spi_pins ms5611)
  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  ms5611SPISensorManager req sensors_ready meas (SPIDeviceHandle 0)



