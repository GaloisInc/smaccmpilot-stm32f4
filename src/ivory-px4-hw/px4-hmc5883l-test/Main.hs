{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.Tower.Frontend

import Ivory.BSP.STM32F4.RCC (BoardHSE)
import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.UART.Tower
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.I2C
import Ivory.BSP.STM32F4.Signalable

import SMACCMPilot.Hardware.HMC5883L

import Platform

main :: IO ()
main = compilePlatforms conf (gpsPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: forall p . (MPU6kPlatform p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (_consIn,_consOut) <- uartTower (consoleUart (Proxy :: Proxy p))
                                115200 (Proxy :: Proxy 128)

  (req, res) <- i2cTower i2c2 pinB10 pinB11

  hmc5883lctl req res (I2CDeviceAddr 0x3C)


hmc5883lctl :: forall p
        . (BoardHSE p, STM32F4Signal p)
       => ChannelSource (Struct "i2c_transaction_request")
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


