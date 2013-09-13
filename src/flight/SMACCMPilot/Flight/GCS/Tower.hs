{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.RCC

gcsTower :: (BoardHSE p)
         => String
         -> UART
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p ()
gcsTower uartname uart fm_sink sens_sink pos_sink ctl_sink motor_sink = do
  (streamrate_source, streamrate_sink) <- channel
  (dataRateSrc,        dataRateSink)   <- channel

  ( (istream :: ChannelSink   1024 (Stored Uint8))
   ,(ostream :: ChannelSource 1024 (Stored Uint8))) <- uartTower uart 57600

  task ("gcsReceiveTask" ++ uartname) $
    gcsReceiveTask istream streamrate_source dataRateSrc
  task ("gcsTransmitTask" ++ uartname) $
    gcsTransmitTask ostream streamrate_sink dataRateSink fm_sink sens_sink
      pos_sink ctl_sink motor_sink
