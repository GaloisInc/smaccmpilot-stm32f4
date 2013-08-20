{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language

import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

import Ivory.BSP.STM32F4.UART

gcsTower :: String
         -> UART
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p ()
gcsTower uartname uart fm_sink sens_sink pos_sink ctl_sink motor_sink = do
  (streamrate_source, streamrate_sink) <- channel
  (dataRateSrc, dataRateSink)          <- channel

  (i :: Channel 128 (Stored Uint8)) <- channelWithSize
  (o :: Channel 128 (Stored Uint8)) <- channelWithSize
  uartTower uart 57600 (snk i) (src o)

  task ("gcsReceiveTask" ++ uartname) $
    gcsReceiveTask (snk o) streamrate_source dataRateSrc
  task ("gcsTransmitTask" ++ uartname) $
    gcsTransmitTask (src i) streamrate_sink dataRateSink fm_sink sens_sink
      pos_sink ctl_sink motor_sink
