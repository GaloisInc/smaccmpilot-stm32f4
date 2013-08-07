{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language

import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

gcsTower :: String -> MemArea (Struct "usart")
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "servos")
         -> Tower p ()
gcsTower usartname usart fm_sink sens_sink pos_sink ctl_sink servo_sink = do
  (streamrate_source, streamrate_sink) <- channel
  (dataRateSrc, dataRateSink)          <- channel
  task ("gcsReceiveTask" ++ usartname) $
    gcsReceiveTask usart streamrate_source dataRateSrc
  task ("gcsTransmitTask" ++ usartname) $
    gcsTransmitTask usart streamrate_sink dataRateSink fm_sink sens_sink
      pos_sink ctl_sink servo_sink
