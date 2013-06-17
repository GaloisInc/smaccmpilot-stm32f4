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
         -> Tower ()
gcsTower usartname usart fm_sink sens_sink pos_sink ctl_sink servo_sink = do
  (streamrate_source, streamrate_sink) <- channel
  task ("gcsReceiveTask" ++ usartname) $
    gcsReceiveTask usart streamrate_source
  task ("gcsTransmitTask" ++ usartname) $
    gcsTransmitTask usart streamrate_sink fm_sink sens_sink
      pos_sink ctl_sink servo_sink
