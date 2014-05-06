{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language
import Ivory.Stdlib

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

import SMACCMPilot.Hardware.MS5611.I2C

import Platform

main :: IO ()
main = compilePlatforms conf (gpsPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: forall p . (MPU6kPlatform p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  --towerModule  rawSensorTypeModule
  --towerDepends rawSensorTypeModule

  --raw_sensor <- channel

  (_consIn,_consOut) <- uartTower (consoleUart (Proxy :: Proxy p))
                                115200 (Proxy :: Proxy 128)

  (req, res) <- i2cTower i2c2 pinB10 pinB11

  -- 0b01110110 = 0x76
  ms5611ctl req res (I2CDeviceAddr 0x76)

  -- XXX hook snk raw_sensor up to console.

ms5611ctl :: forall p
        . (BoardHSE p, STM32F4Signal p)
       => ChannelSource (Struct "i2c_transaction_request")
       -> ChannelSink   (Struct "i2c_transaction_result")
       -> I2CDeviceAddr
       -> Tower p ()
ms5611ctl toDriver fromDriver addr = task "ms5611ctl" $ do
  i2cRequest <- withChannelEmitter toDriver "i2cRequest"
  i2cResult <- withChannelEvent fromDriver "i2cResult"
  --sensorEmitter <- withChannelEmitter sensorSource "sensorOutput"

  (driver, deviceprops) <- driverMachine addr i2cRequest i2cResult
                              (\_ _ _ -> return ()) -- XXX
  taskStackSize 3072

  taskInit $ do
    begin driver
