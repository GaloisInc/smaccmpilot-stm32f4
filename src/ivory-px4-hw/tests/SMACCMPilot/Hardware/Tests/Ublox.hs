{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.Tests.Ublox
  ( app
  , uartUbloxGPSTower
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.UART.DMA
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import SMACCMPilot.Hardware.GPS.UBlox

import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Serialize

[ivory| string struct UnusedString 1 |]
[ivory| string struct GPSString 128 |]

uartUbloxGPSTower :: (e -> ClockConfig)
                  -> UART_Device
                  -> ChanInput ('Struct "position_sample")
                  -> Tower e (Monitor e ())
uartUbloxGPSTower tocc uart ostream = do
  let unusedmod = package "unused_string" $ defStringType (Proxy :: Proxy UnusedString)
  let gpsstringmod = package "gps_string" $ defStringType (Proxy :: Proxy GPSString)
  towerModule unusedmod
  towerDepends unusedmod
  towerModule gpsstringmod
  towerDepends gpsstringmod
  (_gpso :: BackpressureTransmit UnusedString ('Stored IBool), gpsi, mon) <-
    case uart_periph uart of
      Left u -> uartTower tocc u (uart_pins uart) 38400
      Right dmauart -> dmaUARTTower tocc dmauart (uart_pins uart) 38400 (Proxy :: Proxy GPSString)
  ubloxGPSTower gpsi ostream
  return mon

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (uarto, _uarti, mon1) <- px4ConsoleTower topx4

  let gps = px4platform_gps px4platform
  position <- channel
  mon2 <- uartUbloxGPSTower (px4platform_clockconfig . topx4) gps (fst position)
  monitor "uart_dma" (mon1 >> mon2)
  monitor "positionSender" $ do
    positionSender (snd position) uarto

  serializeTowerDeps


