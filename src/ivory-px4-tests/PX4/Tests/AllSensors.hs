{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.AllSensors (app) where

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

import           PX4.Tests.Platforms
import           PX4.Tests.MPU6000  (mpu6000SensorManager, mpu6000Sender)
import           PX4.Tests.MS5611   (ms5611Sender)
import           PX4.Tests.HMC5883L (hmc5883lSender)
import           PX4.Tests.Ublox    (positionSender)

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  boardInitializer

  (gpsi, _gpso) <- uartTower (gpsUart (Proxy :: Proxy p))
                                38400 (Proxy :: Proxy 128)
  position <- channel
  U.ubloxGPSTower gpsi (src position)

  (ireq, ires) <- i2cTower (ms5611periph platform)
                           (ms5611sda platform)
                           (ms5611scl platform)
  ms5611meas     <- channel
  hmc5883lsample <- channel
  i2cSensorManager ireq ires (src ms5611meas) (src hmc5883lsample)

  mpu6000sample <- channel
  (sreq, sres) <- spiTower [mpu6000Device platform]
  mpu6000SensorManager sreq sres (src mpu6000sample) (SPIDeviceHandle 0)

  (_uarti,uarto) <- uartTower (consoleUart platform) 115200 (Proxy :: Proxy 128)

  task "sensorsender" $ do
    uartout <- withChannelEmitter uarto "uartout"
    hmc5883lSender (snk hmc5883lsample) uartout
    ms5611Sender   (snk ms5611meas)     uartout
    mpu6000Sender  (snk mpu6000sample)  uartout
    positionSender (snk position)       uartout

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
  platform = Proxy :: Proxy p

i2cSensorManager :: forall p . (TestPlatform p)
              => ChannelSource (Struct "i2c_transaction_request")
              -> ChannelSink   (Struct "i2c_transaction_result")
              -> ChannelSource (Struct "ms5611_measurement")
              -> ChannelSource (Struct "hmc5883l_sample")
              -> Tower p ()
i2cSensorManager toDriver fromDriver ms5611MeasSource hmc5883lSampleSource =
  task "i2cSensorManager" $ do
    i2cRequest      <- withChannelEmitter toDriver "i2cRequest"
    i2cResult       <- withChannelEvent   fromDriver "i2cResult"
    ms5611Emitter   <- withChannelEmitter ms5611MeasSource "ms5611Meas"
    hmc5883lEmitter <- withChannelEmitter hmc5883lSampleSource "hmc5883lSample"

    driver <- i2cSensorMachine i2cRequest i2cResult
                ms5611Emitter hmc5883lEmitter

    taskStackSize 4096

    taskInit $ do
      begin driver

i2cSensorMachine :: forall p . (TestPlatform p)
                 => ChannelEmitter (Struct "i2c_transaction_request")
                 -> Event          (Struct "i2c_transaction_result")
                 -> ChannelEmitter (Struct "ms5611_measurement")
                 -> ChannelEmitter (Struct "hmc5883l_sample")
                 -> Task p Runnable
i2cSensorMachine i2cRequest i2cResult ms5611MeasEmitter hmc5883lSampleEmitter = do
  h     <- taskLocal "hmc5883l_sample"
  m_cal <- taskLocal "ms5611_calibration"
  m     <- taskLocal "ms5611_measurement"
  stateMachine "multiSensorDriver" $ mdo
    m_setup <- M.sensorSetup m_addr (m ~> M.initfail) m_cal i2cRequest i2cResult h_setup
    h_setup <- H.sensorSetup h_addr (h ~> H.initfail)       i2cRequest i2cResult m_read

    m_read  <- M.sensorRead  m_addr (constRef m_cal) m i2cRequest i2cResult h_read
    h_read  <- H.sensorRead  h_addr h                  i2cRequest i2cResult waitRead

    waitRead <- stateNamed "waitRead" $ do
      entry $ liftIvory_ $ do
        emit_ ms5611MeasEmitter     (constRef m)
        emit_ hmc5883lSampleEmitter (constRef h)
      timeout (Milliseconds 13) $ -- XXX 75hz?
        goto m_read

    return m_setup

  where
  platform = Proxy :: Proxy p
  m_addr = ms5611addr platform
  h_addr = hmc5883addr platform

