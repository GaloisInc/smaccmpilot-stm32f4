{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.Ublox
  ( app
  ) where

import Ivory.Language

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.GPS.UBlox

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (_uarti, uarto) <- px4ConsoleTower topx4

  let gps = px4platform_gps px4platform
  (gpsi, _gpso) <- uartTower (px4platform_clockconfig . topx4)
                             (uart_periph gps)
                             (uart_pins gps)
                             38400
                             (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)
  monitor "positionSender" $ do
    positionSender (snd position) uarto

  serializeTowerDeps


