{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
import Ivory.BSP.STM32F4.SPI
import Ivory.BSP.STM32F4.Signalable

import SMACCMPilot.Hardware.MPU6000.SPI

import Platform

main :: IO ()
main = compilePlatforms conf (gpsPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: forall p . (MPU6kPlatform p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (_consIn,_consOut) <- uartTower (consoleUart (Proxy :: Proxy p))
                                115200 (Proxy :: Proxy 128)

  (req, res) <- spiTower [mpu6000Device (Proxy :: Proxy p)]

  mpu6kCtl req res (SPIDeviceHandle 0)

mpu6kCtl :: forall p
        . (BoardHSE p, STM32F4Signal p)
       => ChannelSource (Struct "spi_transaction_request")
       -> ChannelSink   (Struct "spi_transaction_result")
       -> SPIDeviceHandle
       -> Tower p ()
mpu6kCtl toDriver fromDriver dh = task "mpu6kCtl" $ do
  spiRequest <- withChannelEmitter toDriver "toDriver"
  spiResult <- withChannelEvent fromDriver "fromDriver"

  (initMachine, initError) <- initializerMachine dh spiRequest spiResult

  taskInit $ do
    begin initMachine

  p <- withPeriodicEvent (Milliseconds 20)
  handle p "period" $ \_ -> do
    initializing <- active initMachine
    e <- deref initError
    when (iNot initializing .&& iNot e) $ do
      return ()
      -- XXX

