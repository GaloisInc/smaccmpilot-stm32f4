{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower where

import Ivory.Language

import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

gcsTower :: (SingI n, SingI m)
         => String
         -> ChannelSink n (Stored Uint8)
         -> ChannelSource m (Stored Uint8) 
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p ()
gcsTower name istream ostream fm_sink sens_sink pos_sink ctl_sink motor_sink = do
  (streamrate_source, streamrate_sink) <- channel
  (dataRateSrc,        dataRateSink)   <- channel

  task ("gcsReceiveTask" ++ name) $
    gcsReceiveTask istream streamrate_source dataRateSrc
  task ("gcsTransmitTask" ++ name) $
    gcsTransmitTask ostream streamrate_sink dataRateSink fm_sink sens_sink
      pos_sink ctl_sink motor_sink
