{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MPU6000
  ( mpu6000SensorManager
  , mpu6000Sender
  , app
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize
import qualified Ivory.HXStream as HX

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MPU6000

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  sample <- channel
  px4platform <- fmap topx4 getEnv
  let mpu6000 = px4platform_mpu6000_device px4platform
  (req, res) <- spiTower tocc [mpu6000]
  mpu6000SensorManager req res (fst sample) (SPIDeviceHandle 0)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti, uarto) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 256)
  monitor "mpu6000sender" $ do
    mpu6000Sender (snd sample) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  towerModule  mpu6000TypesModule
  towerDepends mpu6000TypesModule
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

mpu6000Sender :: ChanOutput (Struct "mpu6000_sample")
              -> ChanInput (Stored Uint8)
              -> Monitor e ()
mpu6000Sender meas out = do
  (buf :: Ref Global (Array 38 (Stored Uint8))) <- state "mpu6000_ser_buf"
  handler meas "measurement" $ do
    e <- emitter out (38*2 + 3)
    callback $ \s -> noReturn $ do
      packInto_ buf 0 $ mpack s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 103 -- 'g' for gyro

mpu6000SensorManager :: ChanInput (Struct "spi_transaction_request")
                     -> ChanOutput (Struct "spi_transaction_result")
                     -> ChanInput (Struct "mpu6000_sample")
                     -> SPIDeviceHandle
                     -> Tower e ()
mpu6000SensorManager spiRequest spiResult sensorChan dh = do
  p <- period (Milliseconds 5) -- 200hz
  monitor "mpu6kCtl" $ do
    (initMachine, initError) <- initializerMachine dh spiRequest spiResult
    transactionPending <- stateInit "transactionPending" (ival false)
    result             <- state     "mpu6000_result"

    handler p "period" $ do
      sensorEmitter <- emitter sensorChan 1
      spiEmitter    <- emitter spiRequest 1
      callback $ \_ -> do
        initializing <- stateMachine_active initMachine
        running      <- deref transactionPending
        initerror    <- deref initError
        cond_
          [ initializing .|| (iNot initializing .&& initerror) ==> do
              store (result ~> initfail) true
              invalidTransaction result
              emit sensorEmitter (constRef result)
          , running ==> do
              invalidTransaction result
              emit sensorEmitter (constRef result)
          , true ==> do
              store (result ~> initfail) false
              store transactionPending true
              req <- getSensorsReq dh
              emit spiEmitter req
          ]

    handler spiResult "spiResult" $ do
      sensorEmitter <- emitter sensorChan 1
      callback $ \res -> do
        initializing <- stateMachine_active initMachine
        unless initializing $ do
          store transactionPending false
          rawSensorFromResponse res result
          emit sensorEmitter (constRef result)

  where
  invalidTransaction :: (GetAlloc eff ~ Scope s)
                => Ref s' (Struct "mpu6000_sample") -> Ivory eff ()
  invalidTransaction r = do
    t <- getTime
    store (r ~> samplefail) true
    store (r ~> gyro_x)     0
    store (r ~> gyro_y)     0
    store (r ~> gyro_z)     0
    store (r ~> accel_x)    0
    store (r ~> accel_y)    0
    store (r ~> accel_z)    0
    store (r ~> temp)       0
    store (r ~> time)       t

