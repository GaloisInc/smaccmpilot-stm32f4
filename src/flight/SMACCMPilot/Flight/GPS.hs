{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.GPS (gpsTower) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Driver.UART

import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable

import SMACCMPilot.Hardware.GPS.UBlox

gpsTower :: (PlatformClock p, STM32Signal p)
         => UART (InterruptType p)
         -> Tower p (ChannelSink (Struct "position"))
gpsTower uart = do
  (gpsi,_gpso) <- uartTower uart 38400 (Proxy :: Proxy 256)
  position <- channel
  ubloxGPSTower gpsi (src position)
  return (snk position)

