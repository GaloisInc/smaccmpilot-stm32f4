{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.AllSensors
  ( app
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
import PX4.Tests.MS5611   (ms5611Sender, ms5611ctl)
import PX4.Tests.HMC5883L (hmc5883lSender, hmc5883lctl)
import PX4.Tests.Ublox    (positionSender)

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let gps = px4platform_gps_device px4platform
  (gpsi, _gpso) <- uartTower tocc gps
                                38400 (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)

  let hmc = px4platform_hmc5883_device px4platform
  (ireq, ires, iready) <- i2cTower tocc
                         (hmc5883device_periph hmc)
                         (hmc5883device_sda    hmc)
                         (hmc5883device_scl    hmc)
  (ms5611meas, hmc5883lsample) <- i2cSensorManager px4platform iready ireq ires

  mpu6000sample <- channel
  let mpu6000 = px4platform_mpu6000_device px4platform
      pins    = px4platform_mpu6000_spi_pins px4platform
  (sreq, sres, sready) <- spiTower tocc [mpu6000] pins
  mpu6000SensorManager sreq sres sready (fst mpu6000sample) (SPIDeviceHandle 0)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti,uartout) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 128)

  monitor "sensorsender" $ do
    hmc5883lSender hmc5883lsample       uartout
    ms5611Sender   ms5611meas           uartout
    -- XXX: mpu6000 produces too much output for a 115200 bps UART
    -- mpu6000Sender  (snd mpu6000sample)  uartout
    positionSender (snd position)       uartout

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

i2cSensorManager :: PX4Platform i
                 -> ChanOutput (Stored ITime)
                 -> ChanInput  (Struct "i2c_transaction_request")
                 -> ChanOutput (Struct "i2c_transaction_result")
                 -> Tower e ( ChanOutput (Struct "ms5611_measurement")
                            , ChanOutput (Struct "hmc5883l_sample"))
i2cSensorManager p i2cReady i2cRequest i2cResult = do
  (ms5611task, ms5611Req, ms5611Res) <- task "ms5611"
  ms5611Chan <- ms5611ctl ms5611Req ms5611Res m_addr

  (hmc5883task, hmc5883Req, hmc5883Res) <- task "hmc5883"
  hmc5883Chan <- hmc5883lctl hmc5883Req hmc5883Res h_addr

  schedule [ms5611task, hmc5883task] i2cReady i2cRequest i2cResult

  return (ms5611Chan, hmc5883Chan)
  where
  m_addr = ms5611device_addr (px4platform_ms5611_device p)
  h_addr = hmc5883device_addr (px4platform_hmc5883_device p)
