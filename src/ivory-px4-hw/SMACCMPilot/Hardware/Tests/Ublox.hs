{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.Ublox
  ( app
  , uartUbloxGPSTower
  ) where

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Hardware.GPS.UBlox

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

uartUbloxGPSTower :: (e -> ClockConfig)
                  -> UART_Device
                  -> ChanInput (Struct "position")
                  -> Tower e ()
uartUbloxGPSTower tocc uart ostream = do
  (gpsi, _gpso) <- uartTower tocc
                             (uart_periph uart)
                             (uart_pins uart)
                             38400
                             (Proxy :: Proxy 128)
  ubloxGPSTower gpsi ostream

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  (_uarti, uarto) <- px4ConsoleTower topx4

  let gps = px4platform_gps px4platform
  position <- channel
  uartUbloxGPSTower (px4platform_clockconfig . topx4) gps (fst position)
  monitor "positionSender" $ do
    positionSender (snd position) uarto

  serializeTowerDeps


