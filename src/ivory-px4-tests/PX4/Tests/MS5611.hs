{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MS5611 (app) where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C

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
  ms5611ctl req res (ms5611addr platform)
  where
  platform = Proxy :: Proxy p

ms5611ctl :: ChannelSource (Struct "i2c_transaction_request")
          -> ChannelSink   (Struct "i2c_transaction_result")
          -> I2CDeviceAddr
          -> Tower p ()
ms5611ctl toDriver fromDriver addr = task "ms5611ctl" $ do
  i2cRequest <- withChannelEmitter toDriver "i2cRequest"
  i2cResult <- withChannelEvent fromDriver "i2cResult"

  calibration <- taskLocal "calibration"
  sample      <- taskLocal "sample"
  meas        <- taskLocal "meas"

  driver <- testDriverMachine addr i2cRequest i2cResult
              calibration sample meas (meas ~> initfail) (meas ~> sampfail)

  taskStackSize 3072

  taskInit $ do
    begin driver

testDriverMachine :: I2CDeviceAddr
                  -> ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> Ref Global (Struct "ms5611_calibration")
                  -> Ref Global (Struct "ms5611_sample")
                  -> Ref Global (Struct "ms5611_measurement")
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> Task p Runnable
testDriverMachine addr i2cRequest i2cResult calibration sample meas ifail sfail =
  stateMachine "ms5611TestDriver" $ mdo
    setup <- sensorSetup  addr ifail calibration i2cRequest i2cResult read
    read  <- sensorSample addr sfail sample      i2cRequest i2cResult m
    m     <- stateNamed "ms5611_measurement" $ entry $ do
      liftIvory_ $ measurement (constRef calibration) (constRef sample) meas
      goto read
    return setup


