{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Flight.GPS (gpsTower) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.UART.Tower
import qualified Ivory.BSP.STM32F405.Interrupt as F405

import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable

import SMACCMPilot.Hardware.GPS.UBlox

gpsTower :: (PlatformClock p, STM32Signal F405.Interrupt p)
         => UART F405.Interrupt -> Tower p (ChannelSink (Struct "position"))
gpsTower uart = do
  (gpsi,_gpso) <- uartTower uart 38400 (Proxy :: Proxy 256)
  position <- channel
  ubloxGPSTower gpsi (src position)
  return (snk position)

