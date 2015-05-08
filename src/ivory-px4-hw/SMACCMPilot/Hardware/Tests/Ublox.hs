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
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import SMACCMPilot.Hardware.GPS.UBlox

import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

[ivory| string struct UnusedString 1 |]

uartUbloxGPSTower :: (e -> ClockConfig)
                  -> UART_Device
                  -> ChanInput (Struct "position")
                  -> Tower e ()
uartUbloxGPSTower tocc uart ostream = do
  let unusedmod = package "unused_string" $ defStringType (Proxy :: Proxy UnusedString)
  towerModule unusedmod
  towerDepends unusedmod
  (_gpso :: BackpressureTransmit UnusedString (Stored IBool), gpsi)
    <- uartTower tocc (uart_periph uart) (uart_pins uart) 38400
  ubloxGPSTower gpsi ostream

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (uarto, _uarti) <- px4ConsoleTower topx4

  let gps = px4platform_gps px4platform
  position <- channel
  uartUbloxGPSTower (px4platform_clockconfig . topx4) gps (fst position)
  monitor "positionSender" $ do
    positionSender (snd position) uarto

  serializeTowerDeps


