{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GPS (gpsTower) where

import Ivory.Language
import Ivory.Tower
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.UART
import SMACCMPilot.Hardware.GPS.UBlox

gpsTower :: (BoardHSE p) => UART -> Tower p (ChannelSink 16 (Struct "position"))
gpsTower uart = do
  (gpsi :: ChannelSink 256 (Stored Uint8)
   ,_gpso :: ChannelSource 2 (Stored Uint8)) <- uartTower uart 38400
  position <- channel
  ubloxGPSTower gpsi (src position)
  return (snk position)

