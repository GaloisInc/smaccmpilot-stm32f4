{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.SensorFusion
  ( app
  ) where

import qualified BSP.Tests.Platforms as BSP
import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART
import qualified Ivory.BSP.STM32F405.Interrupt as F405
import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import PX4.Tests.Platforms
import PX4.Tests.AllSensors (i2cSensorManager)
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import SMACCMPilot.Hardware.GPS.UBlox
import SMACCMPilot.Hardware.MPU6000
import SMACCMPilot.INS.Tower

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let gps_periph = px4platform_gps_device px4platform
      gps_pins = px4platform_gps_pins px4platform
  (gpsi, _gpso) <- uartTower tocc gps_periph gps_pins
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
      spi_pins = px4platform_mpu6000_spi_pins px4platform
  (sreq, sres, sready) <- spiTower tocc [mpu6000] spi_pins
  mpu6000SensorManager sreq sres sready (fst mpu6000sample) (SPIDeviceHandle 0)

  states <- sensorFusion (snd mpu6000sample) hmc5883lsample ms5611meas (snd position)

  let u = BSP.testplatform_uart (px4platform_testplatform px4platform)
  (_uarti, uartout) <- uartTower tocc (BSP.testUARTPeriph u) (BSP.testUARTPins u)
                               115200 (Proxy :: Proxy 256)

  p <- period (Milliseconds 40) -- can't send states much faster than 25Hz at 115200bps

  monitor "sensorsender" $ do
    last_state <- state "last_state"
    handler states "buffer_state" $ callback $ refCopy last_state

    (buf :: Ref Global (Array 88 (Stored Uint8))) <- state "state_ser_buf"
    handler p "send_state" $ do
      e <- emitter uartout (2 * 88 + 3) -- twice buf size plus tag and two fbos
      callback $ const $ noReturn $ do
        packInto buf 0 $ constRef last_state
        let tag = 102 -- 'f' for fusion
        HX.encode tag (constRef buf) (emitV e)

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

