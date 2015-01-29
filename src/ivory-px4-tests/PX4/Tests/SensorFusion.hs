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
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import SMACCMPilot.Hardware.GPS.UBlox
import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.MPU6000
import SMACCMPilot.Hardware.MS5611
import SMACCMPilot.Hardware.Sched
import SMACCMPilot.INS.Tower

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
  (sreq, sres, sready) <- spiTower tocc [mpu6000]
  mpu6000SensorManager sreq sres sready (fst mpu6000sample) (SPIDeviceHandle 0)

  states <- sensorFusion (snd mpu6000sample) hmc5883lsample ms5611meas (snd position)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti,uartout) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 256)

  p <- period (Milliseconds 40) -- can't send states much faster than 25Hz at 115200bps

  monitor "sensorsender" $ do
    last_state <- state "last_state"
    handler states "buffer_state" $ callback $ refCopy last_state

    (buf :: Ref Global (Array 88 (Stored Uint8))) <- state "state_ser_buf"
    handler p "send_state" $ do
      e <- emitter uartout (2 * 88 + 3) -- twice buf size plus tag and two fbos
      callback $ const $ noReturn $ do
        packInto_ buf 0 $ mpack $ constRef last_state
        let tag = 102 -- 'f' for fusion
        HX.encode tag (constRef buf) (emitV e)

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
