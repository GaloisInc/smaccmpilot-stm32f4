{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.Connections.FreeRTOS

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

gcsTower :: MemArea (Struct "usart")
         -> Sink (Struct "flightmode")
         -> IvoryTower ()
gcsTower usart flightmode_sink = do
  (streamrate_source, streamrate_sink) <- connector sharedState
  addTask $ gcsReceiveTask  usart streamrate_source
  addTask $ gcsTransmitTask usart streamrate_sink flightmode_sink
