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
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import qualified SMACCMPilot.Hardware.MPU6000        as G
import qualified SMACCMPilot.Hardware.MS5611         as M
import qualified SMACCMPilot.Hardware.HMC5883L       as H
import qualified SMACCMPilot.Hardware.GPS.UBlox      as U

import           PX4.Tests.MPU6000  (mpu6000SensorManager, mpu6000Sender)
import           PX4.Tests.MS5611   (ms5611Sender)
import           PX4.Tests.HMC5883L (hmc5883lSender)
import           PX4.Tests.Ublox    (positionSender)

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
  U.ubloxGPSTower gpsi (fst position)

  let hmc = px4platform_hmc5883_device px4platform
  (ireq, ires) <- i2cTower tocc
                         (hmc5883device_periph hmc)
                         (hmc5883device_sda    hmc)
                         (hmc5883device_scl    hmc)
  ms5611meas     <- channel
  hmc5883lsample <- channel
  i2cSensorManager px4platform ireq ires (fst ms5611meas) (fst hmc5883lsample)

  mpu6000sample <- channel
  let mpu6000 = px4platform_mpu6000_device px4platform
  (sreq, sres) <- spiTower tocc [mpu6000]
  mpu6000SensorManager sreq sres (fst mpu6000sample) (SPIDeviceHandle 0)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti,uartout) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 128)

  monitor "sensorsender" $ do
    hmc5883lSender (snd hmc5883lsample) uartout
    ms5611Sender   (snd ms5611meas)     uartout
    mpu6000Sender  (snd mpu6000sample)  uartout
    positionSender (snd position)       uartout

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  towerModule  G.mpu6000TypesModule
  towerDepends G.mpu6000TypesModule
  towerModule  M.ms5611TypesModule
  towerDepends M.ms5611TypesModule
  towerModule  H.hmc5883lTypesModule
  towerDepends H.hmc5883lTypesModule
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

i2cSensorManager :: PX4Platform i
                 -> ChanInput  (Struct "i2c_transaction_request")
                 -> ChanOutput (Struct "i2c_transaction_result")
                 -> ChanInput  (Struct "ms5611_measurement")
                 -> ChanInput  (Struct "hmc5883l_sample")
                 -> Tower e ()
i2cSensorManager p i2cRequest i2cResult ms5611Chan hmc5883Chan =
  monitor "i2cSensorManager" $ do
    driver <- i2cSensorMachine i2cRequest i2cResult
                m_addr ms5611Chan h_addr hmc5883Chan
    stateMachine_onChan driver i2cResult
  where
  m_addr = ms5611device_addr (px4platform_ms5611_device p)
  h_addr = hmc5883device_addr (px4platform_hmc5883_device p)

i2cSensorMachine :: ChanInput (Struct "i2c_transaction_request")
                 -> ChanOutput (Struct "i2c_transaction_result")
                 -> I2CDeviceAddr
                 -> ChanInput (Struct "ms5611_measurement")
                 -> I2CDeviceAddr
                 -> ChanInput (Struct "hmc5883l_sample")
                 -> Monitor e (StateMachine e)
i2cSensorMachine i2cRequest i2cResult m_addr ms5611Chan h_addr hmc5883Chan = do
  h     <- state "hmc5883l_sample"
  m_cal <- state "ms5611_calibration"
  m     <- state "ms5611_measurement"
  stateMachine "multiSensorDriver" $ mdo
    m_setup <- M.sensorSetup m_addr (m ~> M.initfail) m_cal i2cRequest i2cResult h_setup
    h_setup <- H.sensorSetup h_addr (h ~> H.initfail)       i2cRequest i2cResult m_read

    m_read  <- M.sensorRead  m_addr (constRef m_cal) m i2cRequest i2cResult h_read
    h_read  <- H.sensorRead  h_addr h                  i2cRequest i2cResult waitRead

    waitRead <- machineStateNamed "waitRead" $ do
      entry $ do
        ms5611Emitter <- machineEmitter ms5611Chan 1
        hmc5883Emitter <- machineEmitter hmc5883Chan 1
        machineCallback $ \_ -> do
          emit ms5611Emitter (constRef m)
          emit hmc5883Emitter (constRef h)
      timeout (Milliseconds 13) $ -- XXX 75hz?
        machineControl $ \_ -> return $ goto m_read

    return m_setup
