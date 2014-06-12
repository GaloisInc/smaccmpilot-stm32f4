{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.AllSensors (app) where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C

import qualified SMACCMPilot.Hardware.MS5611         as M
import qualified SMACCMPilot.Hardware.HMC5883L       as H

import PX4.Tests.Platforms
import PX4.Tests.HMC5883L.Types as H

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  boardInitializer
  towerModule  M.ms5611TypesModule
  towerDepends M.ms5611TypesModule
  towerModule  H.hmc5883lTypesModule
  towerDepends H.hmc5883lTypesModule
  (req, res) <- i2cTower (ms5611periph platform)
                         (ms5611sda platform)
                         (ms5611scl platform)
  sampleChan <- channel
  sensorControl req res (src sampleChan)
  where
  platform = Proxy :: Proxy p

sensorControl :: forall p . (TestPlatform p)
              => ChannelSource (Struct "i2c_transaction_request")
              -> ChannelSink   (Struct "i2c_transaction_result")
              -> ChannelSource (Struct "hmc5883l_sample")
              -> Tower p ()
sensorControl toDriver fromDriver sampleSource = task "sensorControl" $ do
  i2cRequest <- withChannelEmitter toDriver "i2cRequest"
  i2cResult <- withChannelEvent fromDriver "i2cResult"
  sampleEmitter <- withChannelEmitter sampleSource "sample"

  driver <- testDriverMachine i2cRequest i2cResult sampleEmitter

  taskStackSize 4096

  taskInit $ do
    begin driver

testDriverMachine :: forall p . (TestPlatform p)
                  => ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> ChannelEmitter (Struct "hmc5883l_sample")
                  -> Task p Runnable
testDriverMachine i2cRequest i2cResult sampleEmitter = do
  h <- taskLocal "hmc5883l_sample"
  m_calibration <- taskLocal "ms5611_calibration"
  m_sample      <- taskLocal "ms5611_sample"
  m_ifail       <- taskLocal "ms5611_initfail"
  m_sfail       <- taskLocal "ms5611_samplefail"
  stateMachine "multiSensorDriver" $ mdo
    m_setup <- M.sensorSetup m_addr m_ifail m_calibration i2cRequest i2cResult h_setup
    h_setup <- H.sensorSetup h_addr (h ~> initfail)       i2cRequest i2cResult m_read

    m_read  <- M.sensorRead  m_addr m_sfail m_sample      i2cRequest i2cResult h_read
    h_read  <- H.sensorRead  h_addr (h ~> samplefail) (h ~> sample) i2cRequest i2cResult waitRead

    waitRead <- stateNamed "waitRead" $ do
      entry $
        liftIvory_ (emit_ sampleEmitter (constRef h))
      timeout (Milliseconds 13) $ -- XXX 75hz?
        goto m_read

    return m_setup

  where
  platform = Proxy :: Proxy p
  m_addr = ms5611addr platform
  h_addr = hmc5883addr platform

