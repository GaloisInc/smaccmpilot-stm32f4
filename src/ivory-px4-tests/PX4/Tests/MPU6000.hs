{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MPU6000 (mpu6000SensorManager, mpu6000Sender, testApp) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize
import qualified Ivory.HXStream as HX

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MPU6000

import PX4.Tests.Platforms

testApp :: forall p . (TestPlatform p) => Tower p ()
testApp = do
  boardInitializer

  sample <- channel
  (req, res) <- spiTower [mpu6000Device platform]
  mpu6000SensorManager req res (src sample) (SPIDeviceHandle 0)

  (_uarti, uarto) <- uartTower (consoleUart platform) 115200 (Proxy :: Proxy 256)
  task "mpu6000sender" $ do
    uartout <- withChannelEmitter uarto "uartout"
    mpu6000Sender (snk sample) uartout

  towerDepends serializeModule
  towerModule  serializeModule
  towerModule  mpu6000TypesModule
  towerDepends mpu6000TypesModule
  where
  platform = Proxy :: Proxy p

mpu6000Sender :: ChannelSink (Struct "mpu6000_sample")
              -> ChannelEmitter (Stored Uint8)
              -> Task p ()
mpu6000Sender meassink out = do
  meas <- withChannelEvent meassink "measurement"
  (buf :: Ref Global (Array 24 (Stored Uint8))) <- taskLocal "mpu6000_ser_buf"
  handle meas "measurement" $ \s -> noReturn $ do
    ifail <- deref (s ~> initfail)
    sfail <- deref (s ~> samplefail)
    stime <- deref (s ~> time)
    packInto_ buf 0 $ do
      mpackV (ifail ? ((1 :: Uint8), 0))
      mpackV (sfail ? ((1 :: Uint8), 0))
      mpack  (s ~> gyro_x)
      mpack  (s ~> gyro_y)
      mpack  (s ~> gyro_z)
      mpack  (s ~> accel_x)
      mpack  (s ~> accel_y)
      mpack  (s ~> accel_z)
      mpack  (s ~> temp)
      mpackV (toIMicroseconds stime)
    HX.encode tag (constRef buf) (emitV_ out)
  where
  tag = 103 -- 'g' for gyro

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


  transactionPending <- taskLocalInit "transactionPending" (ival false)
  result             <- taskLocal     "mpu6000_result"
  taskInit $ do
    begin initMachine

  p <- withPeriodicEvent (Milliseconds 5) -- 200hz
  handle p "period" $ \_ -> do
    initializing <- active initMachine
    running      <- deref transactionPending
    initerror    <- deref initError
    cond_
      [ initializing .|| running ==> do
          invalidTransaction result
          emit_ sensorEmitter (constRef result)
      , iNot initializing .&& initerror ==> do
          store (result ~> initfail) true
          invalidTransaction result
          emit_ sensorEmitter (constRef result)
      , true ==> do
          store transactionPending true
          req <- getSensorsReq dh
          emit_ spiRequest req
      ]

  handle spiResult "spiResult" $ \res -> do
    initializing <- active initMachine
    unless initializing $ do
      store transactionPending false
      rawSensorFromResponse res result
      emit_ sensorEmitter (constRef result)

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

