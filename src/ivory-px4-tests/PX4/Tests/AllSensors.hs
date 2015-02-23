{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.AllSensors
  ( app
  , i2cSensorManager
  ) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.Sched
import SMACCMPilot.Hardware.GPS.UBlox

import PX4.Tests.MPU6000  (mpu6000SensorManager)
import PX4.Tests.MS5611   (ms5611Sender, ms5611SensorManager)
import PX4.Tests.HMC5883L (hmc5883lSender, hmc5883lSensorManager)
import PX4.Tests.Ublox    (positionSender)

import PX4.Tests.Platforms

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let gps = px4platform_gps px4platform
  (gpsi, _gpso) <- uartTower (px4platform_clockconfig topx4)
                             (uart_periph gps)
                             (uart_pins gps)
                             38400
                             (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)

  let hmc = px4platform_hmc5883l px4platform
  (ireq, ires, iready) <- i2cTower (px4platform_clockconfig topx4)
                         (hmc5883l_i2c_periph hmc)
                         (hmc5883l_i2c_sda    hmc)
                         (hmc5883l_i2c_scl    hmc)
  (ms5611meas, hmc5883lsample) <- i2cSensorManager px4platform iready ireq ires

  mpu6000sample <- channel
  let mpu6000 = px4platform_mpu6000 px4platform
  (sreq, sres, sready) <- spiTower (px4platform_clockconfig topx4)
                                   [mpu6000_spi_device mpu6000]
                                   (mpu6000_spi_pins mpu6000)

  mpu6000SensorManager sreq sres sready (fst mpu6000sample) (SPIDeviceHandle 0)

  (_uarti,uartout) <- px4ConsoleTower topx4

  monitor "sensorsender" $ do
    hmc5883lSender hmc5883lsample       uartout
    ms5611Sender   ms5611meas           uartout
    -- XXX: mpu6000 produces too much output for a 115200 bps UART
    -- mpu6000Sender  (snd mpu6000sample)  uartout
    positionSender (snd position)       uartout

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

i2cSensorManager :: PX4Platform
                 -> ChanOutput (Stored ITime)
                 -> ChanInput  (Struct "i2c_transaction_request")
                 -> ChanOutput (Struct "i2c_transaction_result")
                 -> Tower e ( ChanOutput (Struct "ms5611_measurement")
                            , ChanOutput (Struct "hmc5883l_sample"))
i2cSensorManager p i2cReady i2cRequest i2cResult = do
  (ms5611task, ms5611Req, ms5611Res) <- task "ms5611"
  ms5611Chan <- channel
  ms5611SensorManager ms5611Req ms5611Res i2cReady (fst ms5611Chan) m_addr

  (hmc5883task, hmc5883Req, hmc5883Res) <- task "hmc5883"
  hmc5883Chan <-channel
  hmc5883lSensorManager hmc5883Req hmc5883Res i2cReady (fst hmc5883Chan) h_addr

  schedule [ms5611task, hmc5883task] i2cReady i2cRequest i2cResult

  return (snd ms5611Chan, snd hmc5883Chan)
  where
  m_addr = ms5611_i2c_addr (px4platform_ms5611 p)
  h_addr = hmc5883l_i2c_addr (px4platform_hmc5883l p)
