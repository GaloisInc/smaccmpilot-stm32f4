{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.AllSensors
  ( app
  , baro_mag_manager
  ) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import SMACCM.Fragment
import SMACCMPilot.Hardware.Sched
import SMACCMPilot.Hardware.GPS.UBlox

import PX4.Tests.MPU6000      (mpu6000SensorManager)
import PX4.Tests.Baro         (ms5611Sender, ms5611I2CSensorManager)
import PX4.Tests.Magnetometer (hmc5883lSender, hmc5883lSensorManager)
import PX4.Tests.Ublox        (positionSender)

import PX4.Tests.Platforms

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let tocc = px4platform_clockconfig . topx4

  let gps = px4platform_gps px4platform
  (gpsi, _gpso) <- uartTower tocc
                             (uart_periph gps)
                             (uart_pins gps)
                             38400
                             (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)


  (baro_meas, mag_meas) <- baro_mag_manager topx4


  mpu6000sample <- channel
  let mpu6000 = px4platform_mpu6000 px4platform
  (sreq, sres, sready) <- spiTower tocc
                                   [mpu6000_spi_device mpu6000]
                                   (mpu6000_spi_pins mpu6000)

  mpu6000SensorManager sreq sres sready (fst mpu6000sample) (SPIDeviceHandle 0)

  (_uarti,uartout) <- px4ConsoleTower topx4

  monitor "sensorsender" $ do
    hmc5883lSender mag_meas             uartout
    ms5611Sender   baro_meas            uartout
    -- XXX: mpu6000 produces too much output for a 115200 bps UART
    -- mpu6000Sender  (snd mpu6000sample)  uartout
    positionSender (snd position)       uartout

  case px4platform_can px4platform of
    Nothing -> return () -- don't send sensor readings to non-existent CAN busses
    Just can -> do
      (_, canReq, _, _) <- canTower tocc (can_periph can) 500000 (can_RX can) (can_TX can)
      fragmentSenderBlind (snd mpu6000sample) 0x001 False canReq (Proxy :: Proxy 38) -- 200Hz, 5 fragments
      fragmentSenderBlind mag_meas 0x006 False canReq (Proxy :: Proxy 22) -- 50Hz, 3 fragments
      fragmentSenderBlind baro_meas 0x009 False canReq (Proxy :: Proxy 18) -- 50Hz, 3 fragments
      fragmentSenderBlind (snd position) 0x00C False canReq (Proxy :: Proxy 46) -- 1Hz?, 6 fragments

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

baro_mag_manager :: (e -> PX4Platform)
                 -> Tower e ( ChanOutput (Struct "ms5611_measurement")
                            , ChanOutput (Struct "hmc5883l_sample"))
baro_mag_manager topx4 = do
  px4platform <- fmap topx4 getEnv
  case (px4platform_baro px4platform, px4platform_mag px4platform) of
    (Baro_MS5611_I2C ms5611_i2c, Mag_HMC5883L_I2C hmc5883l_i2c) ->
        fmu17_baromag topx4 hmc5883l_i2c ms5611_i2c
    _ -> error "AllSensors baro mag manager: case not implemented"


fmu17_baromag :: (e -> PX4Platform)
              -> HMC5883L_I2C
              -> MS5611_I2C
              -> Tower e ( ChanOutput (Struct "ms5611_measurement")
                         , ChanOutput (Struct "hmc5883l_sample"))
fmu17_baromag topx4 hmc5883l ms5611 = do

  (i2cRequest, i2cResult, i2cReady) <- i2cTower
                                           (px4platform_clockconfig . topx4)
                                           (hmc5883l_i2c_periph hmc5883l)
                                           (hmc5883l_i2c_sda    hmc5883l)
                                           (hmc5883l_i2c_scl    hmc5883l)


  (ms5611task, ms5611Req, ms5611Res) <- task "ms5611"
  ms5611Chan <- channel
  ms5611I2CSensorManager ms5611Req ms5611Res i2cReady
                         (fst ms5611Chan) (ms5611_i2c_addr ms5611)

  (hmc5883task, hmc5883Req, hmc5883Res) <- task "hmc5883"
  hmc5883Chan <-channel
  hmc5883lSensorManager hmc5883Req hmc5883Res i2cReady
                        (fst hmc5883Chan) (hmc5883l_i2c_addr hmc5883l)

  schedule [ms5611task, hmc5883task] i2cReady i2cRequest i2cResult

  return (snd ms5611Chan, snd hmc5883Chan)
