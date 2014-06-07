{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

import Ivory.BSP.STM32F405.GPIO
import Ivory.BSP.STM32F405.I2C

import qualified Ivory.BSP.STM32F405.Interrupt as F405

import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.MS5611

import Platform

main :: IO ()
main = compilePlatforms conf (gpsPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: forall p . (MPU6kPlatform p, PlatformClock p, STM32Signal p
                  , InterruptType p ~ F405.Interrupt)
    => Tower p ()
app = do
  towerModule  ms5611TypesModule
  towerDepends ms5611TypesModule
  (req, res) <- i2cTower i2c2 pinB10 pinB11
  ms5611ctl req res (I2CDeviceAddr 0x76)


ms5611ctl :: forall p
        . (PlatformClock p, STM32Signal p, InterruptType p ~ F405.Interrupt)
       => ChannelSource (Struct "i2c_transaction_request")
       -> ChannelSink   (Struct "i2c_transaction_result")
       -> I2CDeviceAddr
       -> Tower p ()
ms5611ctl toDriver fromDriver addr = task "ms5611ctl" $ do
  i2cRequest <- withChannelEmitter toDriver "i2cRequest"
  i2cResult <- withChannelEvent fromDriver "i2cResult"

  calibration <- taskLocal "calibration"
  sample      <- taskLocal "sample"
  initfail    <- taskLocal "initfail"
  samplefail  <- taskLocal "samplefail"

  driver <- testDriverMachine addr i2cRequest i2cResult
              calibration sample initfail samplefail

  taskStackSize 3072

  taskInit $ do
    begin driver

testDriverMachine :: I2CDeviceAddr
                  -> ChannelEmitter (Struct "i2c_transaction_request")
                  -> Event          (Struct "i2c_transaction_result")
                  -> Ref Global (Struct "ms5611_calibration")
                  -> Ref Global (Struct "ms5611_sample")
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> Task p Runnable
testDriverMachine addr i2cRequest i2cResult calibration sample ifail sfail =
  stateMachine "ms5611TestDriver" $ mdo
    setup <- sensorSetup addr ifail calibration i2cRequest i2cResult read
    read  <- sensorRead  addr sfail sample      i2cRequest i2cResult read
    return setup


