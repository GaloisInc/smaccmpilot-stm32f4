{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.HMC5883L (hmc5883lSender, app) where

import Ivory.Language
import Ivory.Serialize

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART
import qualified Ivory.HXStream as HX

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

  samples <- hmc5883lctl req res (hmc5883addr platform)
  (_uarti,uarto) <- uartTower (consoleUart platform) 115200 (Proxy :: Proxy 128)

  task "hmc5883lsender" $ do
    uartout <- withChannelEmitter uarto "uartout"
    hmc5883lSender samples uartout

  towerDepends serializeModule
  towerModule  serializeModule
  where
  platform = Proxy :: Proxy p

hmc5883lSender :: ChannelSink (Struct "hmc5883l_sample")
               -> ChannelEmitter (Stored Uint8)
               -> Task p ()
hmc5883lSender samplesink out = do
  samp <- withChannelEvent samplesink "sample"
  (buf :: Ref Global (Array 22 (Stored Uint8))) <- taskLocal "hmc5883l_ser_buf"
  handle samp "sample" $ \s -> noReturn $ do
    ifail <- deref (s ~> initfail)
    sfail <- deref (s ~> samplefail)
    stime <- deref (s ~> time)
    packInto_ buf 0 $ do
      mpackV (ifail ? ((1 :: Uint8), 0))
      mpackV (sfail ? ((1 :: Uint8), 0))
      mpack ((s ~> sample) ! 0)
      mpack ((s ~> sample) ! 1)
      mpack ((s ~> sample) ! 2)
      mpackV (toIMicroseconds stime)
    HX.encode tag (constRef buf) (emitV_ out)
  where
  tag = 99 -- 'c' for compass

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


