{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.Tests.AllSensors
  ( app
  , sensor_manager
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.CAN.Sched
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.CAN

import SMACCMPilot.Hardware.CANMessages
import SMACCMPilot.Hardware.GPS.UBlox
import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.LSM303D
import SMACCMPilot.Hardware.MS5611
import SMACCMPilot.Hardware.MPU6000

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

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


  (accel_meas, gyro_meas, mag_meas, baro_meas) <- sensor_manager topx4

  (_uarti,uartout) <- px4ConsoleTower topx4

  div_accel_meas <- rateDivider 4 accel_meas
  div_gyro_meas <- rateDivider 4 gyro_meas
  monitor "sensorsender" $ do
    magSender  mag_meas    uartout
    baroSender baro_meas   uartout
    gyroSender div_gyro_meas   uartout
    accelSender div_accel_meas uartout
    positionSender (snd position) uartout

  case px4platform_can px4platform of
    Nothing -> return () -- don't send sensor readings to non-existent CAN busses
    Just can -> do
      (_, canReqMbox1, canReqMbox2, canReqMbox3) <- canTower tocc
            (can_periph can) 500000 (can_RX can) (can_TX can)
      tasks <- sequence
        [ do
            (t, tx) <- canTask
            go tx
            return t
        | go <-
            [ fragmentSenderBlind gyro_meas gyroType
            , fragmentSenderBlind accel_meas accelType
            , fragmentSenderBlind mag_meas magType
            , fragmentSenderBlind baro_meas baroType
            , fragmentSenderBlind (snd position) gpsType
            ]
        ]
      canScheduler [canReqMbox1, canReqMbox2, canReqMbox3] tasks
  serializeTowerDeps

sensor_manager :: (e -> PX4Platform)
               -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                          , ChanOutput (Struct "gyroscope_sample")
                          , ChanOutput (Struct "magnetometer_sample")
                          , ChanOutput (Struct "barometer_sample"))
sensor_manager topx4 = do
  PX4Platform{..} <- fmap topx4 getEnv
  case (px4platform_baro, px4platform_mag) of
    ( Baro_MS5611_I2C ms5611_i2c
     , Mag_HMC5883L_I2C hmc5883l_i2c) ->
        fmu17_sensor_manager topx4 px4platform_mpu6000 hmc5883l_i2c ms5611_i2c
    ( Baro_MS5611_SPI ms5611_spi
     , Mag_LSM303D_SPI lsm303d_spi) ->
        fmu24_sensor_manager topx4 px4platform_mpu6000 lsm303d_spi ms5611_spi
    _ -> error "AllSensors baro mag manager: case not implemented"


fmu17_sensor_manager :: (e -> PX4Platform)
              -> MPU6000_SPI
              -> HMC5883L_I2C
              -> MS5611_I2C
              -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                         , ChanOutput (Struct "gyroscope_sample")
                         , ChanOutput (Struct "magnetometer_sample")
                         , ChanOutput (Struct "barometer_sample"))
fmu17_sensor_manager topx4 mpu6000 hmc5883l ms5611 = do

  (i2cRequest, i2cReady) <- i2cTower
    (px4platform_clockconfig . topx4)
    (hmc5883l_i2c_periph hmc5883l)
    (hmc5883l_i2c_sda    hmc5883l)
    (hmc5883l_i2c_scl    hmc5883l)

  (ms5611task, ms5611Req) <- task "ms5611"
  baro_s <- channel
  ms5611I2CSensorManager ms5611Req i2cReady
                         (fst baro_s) (ms5611_i2c_addr ms5611)

  (hmc5883task, hmc5883Req) <- task "hmc5883"
  mag_s <-channel
  hmc5883lSensorManager hmc5883Req i2cReady
                        (fst mag_s) (hmc5883l_i2c_addr hmc5883l)

  schedule [ms5611task, hmc5883task] i2cReady i2cRequest

  acc_s <- channel
  gyro_s <- channel

  (sreq, sready) <- spiTower tocc
                                   [mpu6000_spi_device mpu6000]
                                   (mpu6000_spi_pins mpu6000)

  mpu6000SensorManager sreq sready
                       (fst gyro_s) (fst acc_s)
                       (SPIDeviceHandle 0)


  return (snd acc_s, snd gyro_s, snd mag_s, snd baro_s)
  where
  tocc = px4platform_clockconfig . topx4

fmu24_sensor_manager :: (e -> PX4Platform)
              -> MPU6000_SPI
              -> LSM303D_SPI
              -> MS5611_SPI
              -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                         , ChanOutput (Struct "gyroscope_sample")
                         , ChanOutput (Struct "magnetometer_sample")
                         , ChanOutput (Struct "barometer_sample"))
fmu24_sensor_manager topx4 mpu6000 lsm303d ms5611 = do

  acc_s <- channel
  gyro_s <- channel

  (sreq, sready) <- spiTower tocc
                                   [ mpu6000_spi_device mpu6000
                                   , lsm303d_spi_device lsm303d
                                   , ms5611_spi_device  ms5611
                                   ]
                                   (mpu6000_spi_pins mpu6000)

  (mpu6000Task, mpu6000Req) <- task "mpu6000"
  mpu6000SensorManager mpu6000Req sready
                       (fst gyro_s) (fst acc_s)
                       (SPIDeviceHandle 0)

  (lsm303dTask, lsm303dReq) <- task "lsm303d"
  mag_s <- channel
  lsm_acc_s <- channel -- output never used!
  lsm303dSPISensorManager lsm303dDefaultConf lsm303dReq sready
                          (fst mag_s) (fst lsm_acc_s) (SPIDeviceHandle 1)

  (ms5611Task, ms5611Req) <- task "ms5611"
  baro_s <- channel
  ms5611SPISensorManager ms5611Req sready
                         (fst baro_s) (SPIDeviceHandle 2)

  schedule [mpu6000Task, lsm303dTask, ms5611Task] sready sreq

  return (snd acc_s, snd gyro_s, snd mag_s, snd baro_s)
  where
  tocc = px4platform_clockconfig . topx4
