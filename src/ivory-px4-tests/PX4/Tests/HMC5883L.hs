{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.HMC5883L (app) where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.HMC5883L

import PX4.Tests.Platforms

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  (_consIn,_consOut) <- uartTower (consoleUart platform)
                                115200 (Proxy :: Proxy 128)

  (req, res) <- i2cTower (hmc5883periph platform)
                         (hmc5883sda platform)
                         (hmc5883scl platform)

  hmc5883lctl req res (hmc5883addr platform)

  where
  platform = Proxy :: Proxy p

hmc5883lctl :: forall p
        . ChannelSource (Struct "i2c_transaction_request")
       -> ChannelSink   (Struct "i2c_transaction_result")
       -> I2CDeviceAddr
       -> Tower p ()
hmc5883lctl toDriver fromDriver addr = task "hmc5883lctl" $ do
  i2cRequest <- withChannelEmitter toDriver "i2cRequest"
  i2cResult <- withChannelEvent fromDriver "i2cResult"

  sample      <- taskLocal "sample"
  initfail    <- taskLocal "initfail"
  samplefail  <- taskLocal "samplefail"

  driver <- testDriverMachine addr i2cRequest i2cResult
              sample initfail samplefail

  taskStackSize 3072

  taskInit $ do
    begin driver

testDriverMachine :: I2CDeviceAddr
                  -> ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> Ref Global (Array 3 (Stored Uint16))
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> Task p Runnable
testDriverMachine addr i2cRequest i2cResult sample ifail sfail =
  stateMachine "hmc5883lTestDriver" $ mdo
    setup <- sensorSetup addr ifail        i2cRequest i2cResult read
    read  <- sensorRead  addr sfail sample i2cRequest i2cResult waitRead
    waitRead <- stateNamed "waitRead" $
      timeout (Milliseconds 13) $ -- XXX 75hz?
        goto read

    return setup


