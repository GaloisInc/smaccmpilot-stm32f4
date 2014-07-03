{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MS5611 (ms5611Sender, app) where

import Ivory.Language
import Ivory.Serialize
import qualified Ivory.HXStream as HX

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MS5611
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)

import PX4.Tests.Platforms

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  boardInitializer
  towerModule  ms5611TypesModule
  towerDepends ms5611TypesModule
  (req, res) <- i2cTower (ms5611periph platform)
                         (ms5611sda platform)
                         (ms5611scl platform)
  measurements <- ms5611ctl req res (ms5611addr platform)

  (_uarti, uarto) <- uartTower (consoleUart platform) 115200 (Proxy :: Proxy 128)
  task "ms5611sender" $ do
    uartout <- withChannelEmitter uarto "uartout"
    ms5611Sender measurements uartout

  towerDepends serializeModule
  towerModule  serializeModule
  where
  platform = Proxy :: Proxy p

ms5611Sender :: ChannelSink (Struct "ms5611_measurement")
             -> ChannelEmitter (Stored Uint8)
             -> Task p ()
ms5611Sender meassink out = do
  meas <- withChannelEvent meassink "measurement"
  (buf :: Ref Global (Array 18 (Stored Uint8))) <- taskLocal "ms5611_ser_buf"
  handle meas "measurement" $ \s -> noReturn $ do
    ifail <- deref (s ~> initfail)
    sfail <- deref (s ~> sampfail)
    stime <- deref (s ~> time)
    packInto_ buf 0 $ do
      mpackV (ifail ? ((1 :: Uint8), 0))
      mpackV (sfail ? ((1 :: Uint8), 0))
      mpack  (s ~> pressure)
      mpack  (s ~> temperature)
      mpackV (toIMicroseconds stime)
    HX.encode tag (constRef buf) (emitV_ out)
  where
  tag = 98 -- 'b' for barometer

ms5611ctl :: ChannelSource (Struct "i2c_transaction_request")
          -> ChannelSink   (Struct "i2c_transaction_result")
          -> I2CDeviceAddr
          -> Tower p (ChannelSink (Struct "ms5611_measurement"))
ms5611ctl toDriver fromDriver addr = do
  measurements <- channel
  task "ms5611ctl" $ do
    i2cRequest <- withChannelEmitter toDriver "i2cRequest"
    i2cResult <- withChannelEvent fromDriver "i2cResult"
    measemitter <- withChannelEmitter (src measurements) "measurement"

    calibration <- taskLocal "calibration"
    sample      <- taskLocal "sample"
    meas        <- taskLocal "meas"

    driver <- testDriverMachine addr i2cRequest i2cResult
                calibration sample meas (meas ~> initfail)
                (meas ~> sampfail) measemitter

    taskStackSize 4096

    taskInit $ do
      begin driver
  return (snk measurements)

testDriverMachine :: I2CDeviceAddr
                  -> ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> Ref Global (Struct "ms5611_calibration")
                  -> Ref Global (Struct "ms5611_sample")
                  -> Ref Global (Struct "ms5611_measurement")
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> ChannelEmitter (Struct "ms5611_measurement")
                  -> Task p Runnable
testDriverMachine addr i2cRequest i2cResult calibration sample meas ifail sfail e =
  stateMachine "ms5611TestDriver" $ mdo
    setup <- sensorSetup  addr ifail calibration i2cRequest i2cResult read
    read  <- sensorSample addr sfail sample      i2cRequest i2cResult m
    m     <- stateNamed "ms5611_measurement" $ entry $ do
      liftIvory_ $ do
        measurement (constRef calibration) (constRef sample) meas
        emit_ e (constRef meas)
      goto read
    return setup


