{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.HMC5883L (app) where

import Control.Monad (void)

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.HMC5883L

import PX4.Tests.Platforms

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  boardInitializer
  towerModule  hmc5883lTypesModule
  towerDepends hmc5883lTypesModule
  (req, res) <- i2cTower (hmc5883periph platform)
                         (hmc5883sda platform)
                         (hmc5883scl platform)

  void $ hmc5883lctl req res (hmc5883addr platform)

  where
  platform = Proxy :: Proxy p

hmc5883lctl :: forall p
        . ChannelSource (Struct "i2c_transaction_request")
       -> ChannelSink   (Struct "i2c_transaction_result")
       -> I2CDeviceAddr
       -> Tower p (ChannelSink (Struct "hmc5883l_sample"))
hmc5883lctl toDriver fromDriver addr = do
  samplechan <- channel
  task "hmc5883lctl" $ do
    i2cRequest <- withChannelEmitter toDriver "i2cRequest"
    i2cResult <- withChannelEvent fromDriver "i2cResult"
    sampleEmitter <- withChannelEmitter (src samplechan) "sample"

    driver <- testDriverMachine addr i2cRequest i2cResult sampleEmitter

    taskStackSize 3072

    taskInit $ do
      begin driver
  return (snk samplechan)

testDriverMachine :: I2CDeviceAddr
                  -> ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> ChannelEmitter (Struct "hmc5883l_sample")
                  -> Task p Runnable
testDriverMachine addr i2cRequest i2cResult sampleEmitter = do
  s <- taskLocal "sample_buffer"
  stateMachine "hmc5883lTestDriver" $ mdo
    setup <- sensorSetup addr (s ~> initfail) i2cRequest i2cResult read
    read  <- sensorRead  addr s               i2cRequest i2cResult waitRead
    waitRead <- stateNamed "waitRead" $ do
      entry $
        liftIvory_ (emit_ sampleEmitter (constRef s))
      timeout (Milliseconds 13) $ -- XXX 75hz?
        goto read

    return setup


