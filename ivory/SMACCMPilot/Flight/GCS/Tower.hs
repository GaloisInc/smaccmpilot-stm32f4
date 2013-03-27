{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Connections.FreeRTOS

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

gcsTower :: MemArea (Struct "usart")
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "servos")
         -> IvoryTower ()
gcsTower usart fm_sink sens_sink pos_sink ctl_sink servo_sink = do
  (streamrate_source, streamrate_sink) <- connector sharedState
  addTask $ gcsReceiveTask  usart streamrate_source
  addTask $ gcsTransmitTask usart streamrate_sink fm_sink sens_sink pos_sink ctl_sink servo_sink
