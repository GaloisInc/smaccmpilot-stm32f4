{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.SensorManager
  ( sensorManager
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.LSM303D
import SMACCMPilot.Hardware.MS5611
import SMACCMPilot.Hardware.MPU6000
import SMACCMPilot.Hardware.L3GD20.SPI

import SMACCMPilot.Hardware.Sensors

import Ivory.BSP.STM32.ClockConfig

sensorManager :: (e -> Sensors)
              -> (e -> ClockConfig)
              -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                         , ChanOutput (Struct "gyroscope_sample")
                         , ChanOutput (Struct "magnetometer_sample")
                         , ChanOutput (Struct "barometer_sample"))
sensorManager tosens tocc = do
  e <- getEnv
  case tosens e of
    FMU17Sensors{} -> fmu17SensorManager (tosens e) tocc
    FMU24Sensors{} -> fmu24SensorManager (tosens e) tocc


fmu17SensorManager :: Sensors
                   -> (e -> ClockConfig)
                   -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                              , ChanOutput (Struct "gyroscope_sample")
                              , ChanOutput (Struct "magnetometer_sample")
                              , ChanOutput (Struct "barometer_sample"))
fmu17SensorManager FMU17Sensors{..} tocc = do

  (i2cRequest, i2cReady) <- i2cTower
                              tocc
                              fmu17sens_i2c_periph
                              fmu17sens_i2c_pins

  (ms5611task, ms5611Req) <- task "ms5611"
  baro_s <- channel
  ms5611I2CSensorManager ms5611Req i2cReady (fst baro_s) fmu17sens_ms5611

  (hmc5883task, hmc5883Req) <- task "hmc5883"
  mag_s <-channel
  hmc5883lSensorManager hmc5883Req i2cReady (fst mag_s) fmu17sens_hmc5883l

  schedule [ms5611task, hmc5883task] i2cReady i2cRequest

  acc_s <- channel
  gyro_s <- channel

  (sreq, sready) <- spiTower tocc [fmu17sens_mpu6000] fmu17sens_spi_pins

  mpu6000SensorManager sreq sready
                       (fst gyro_s) (fst acc_s)
                       (SPIDeviceHandle 0)

  return (snd acc_s, snd gyro_s, snd mag_s, snd baro_s)

fmu17SensorManager _ _ = error "impossible"

fmu24SensorManager :: Sensors
                   -> (e -> ClockConfig)
                   -> Tower e ( ChanOutput (Struct "accelerometer_sample")
                              , ChanOutput (Struct "gyroscope_sample")
                              , ChanOutput (Struct "magnetometer_sample")
                              , ChanOutput (Struct "barometer_sample"))
fmu24SensorManager FMU24Sensors{..} tocc = do

  acc_s <- channel
  gyro_s <- channel

  let devices = [ fmu24sens_mpu6000
                , fmu24sens_lsm303d
                , fmu24sens_ms5611
                , fmu24sens_l3gd20
                ]
  (sreq, sready) <- spiTower tocc devices fmu24sens_spi_pins

  initdone_sready <- channel
  monitor "sensor_enable" $ do
    handler sready "init" $ do
      e <- emitter (fst initdone_sready) 1
      callback $ \t -> do
        fmu24sens_enable
        emit e t

  l3gd20_rdy <- channel
  l3gd20_panic <- channel
  (l3gd20Task, l3gd20Req) <- task "l3gd20"
  l3gd20Disabler l3gd20Req
                 (snd initdone_sready)
                 (fst l3gd20_rdy)
                 (fst l3gd20_panic)
                 (SPIDeviceHandle 3)

  (mpu6000Task, mpu6000Req) <- task "mpu6000"
  mpu6000SensorManager mpu6000Req (snd l3gd20_rdy)
                       (fst gyro_s) (fst acc_s)
                       (SPIDeviceHandle 0)

  (lsm303dTask, lsm303dReq) <- task "lsm303d"
  mag_s <- channel
  lsm_acc_s <- channel -- output never used!
  lsm303dSPISensorManager lsm303dDefaultConf lsm303dReq (snd l3gd20_rdy)
                          (fst mag_s) (fst lsm_acc_s) (SPIDeviceHandle 1)

  (ms5611Task, ms5611Req) <- task "ms5611"
  baro_s <- channel
  ms5611SPISensorManager ms5611Req (snd l3gd20_rdy)
                         (fst baro_s) (SPIDeviceHandle 2)

  schedule [mpu6000Task, lsm303dTask, ms5611Task, l3gd20Task] sready sreq

  return (snd acc_s, snd gyro_s, snd mag_s, snd baro_s)

fmu24SensorManager _ _ = error "impossible"
