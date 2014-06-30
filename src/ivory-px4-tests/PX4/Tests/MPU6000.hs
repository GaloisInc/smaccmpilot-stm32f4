{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MPU6000 (mpu6000SensorManager, testApp) where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.MPU6000

import PX4.Tests.Platforms

testApp :: forall p . (TestPlatform p) => Tower p ()
testApp = do
  boardInitializer

  sample <- channel
  (req, res) <- spiTower [mpu6000Device platform]
  mpu6000SensorManager req res (src sample) (SPIDeviceHandle 0)

  towerModule  mpu6000TypesModule
  towerDepends mpu6000TypesModule
  where
  platform = Proxy :: Proxy p

mpu6000SensorManager :: ChannelSource (Struct "spi_transaction_request")
                     -> ChannelSink   (Struct "spi_transaction_result")
                     -> ChannelSource (Struct "mpu6000_sample")
                     -> SPIDeviceHandle
                     -> Tower p ()
mpu6000SensorManager toDriver fromDriver sensorSource dh = task "mpu6kCtl" $ do
  spiRequest <- withChannelEmitter toDriver "toDriver"
  spiResult <- withChannelEvent fromDriver "fromDriver"
  sensorEmitter <- withChannelEmitter sensorSource "sensorOutput"

  (initMachine, initError) <- initializerMachine dh spiRequest spiResult

  taskStackSize 2048

  taskInit $ do
    begin initMachine

  p <- withPeriodicEvent (Milliseconds 5) -- 200hz
  handle p "period" $ \_ -> do
    initializing <- active initMachine
    unless initializing $ do
      e <- deref initError
      ifte_ e
        (invalidSensor >>= emit_ sensorEmitter)
        (getSensorsReq dh >>= emit_ spiRequest)

  handle spiResult "spiResult" $ \res -> do
    initializing <- active initMachine
    unless initializing $ do
      t <- getTime
      r <- rawSensorFromResponse res t
      emit_ sensorEmitter r

  where
  invalidSensor :: (GetAlloc eff ~ Scope s)
                => Ivory eff (ConstRef (Stack s) (Struct "mpu6000_sample"))
  invalidSensor = do
    t <- getTime
    fmap constRef $ local $ istruct
      [ valid .= ival false, time .= ival t ]

