{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GPS (gpsTower) where

import Ivory.Language
import Ivory.Tower
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Tower
import Ivory.BSP.STM32F4.Signalable
import SMACCMPilot.Hardware.GPS.UBlox

gpsTower :: (BoardHSE p, STM32F4Signal p)
         => UART -> Tower p (ChannelSink (Struct "position"))
gpsTower uart = do
  (gpsi,_gpso) <- uartTower uart 38400 (Proxy :: Proxy 256)
  position <- channel
  ubloxGPSTower gpsi (src position)
  return (snk position)

