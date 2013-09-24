{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower
  ( gcsTower
  , gcsTowerHil
  ) where

import Ivory.Language

import Ivory.Tower

import qualified SMACCMPilot.Mavlink.Messages.HilState as HIL

import SMACCMPilot.Flight.GCS.HIL
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
gcsTower name istream ostream fm sens pos ctl motor = do
  gcsTowerAux name istream ostream fm sens pos ctl motor >> return ()


gcsTowerHil :: (SingI n, SingI m)
         => String
         -> ChannelSink n (Stored Uint8)
         -> ChannelSource m (Stored Uint8) 
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> ( ChannelSource 16 (Struct "sensors_result")
            , ChannelSink 16 (Struct "sensors_result"))
         -> Tower p ()
gcsTowerHil name istream ostream fm ctl motor sensors = do
  sensors_state <- stateProxy (snk sensors)
  position      <- dataport
  hil <- gcs fm sensors_state (snk position) ctl motor
  task "hilTranslator" $ hilTranslator hil (src sensors) (src position)
  where
  gcs = gcsTowerAux name istream ostream

gcsTowerAux :: (SingI n, SingI m)
         => String
         -> ChannelSink n (Stored Uint8)
         -> ChannelSource m (Stored Uint8) 
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p (ChannelSink 4 (Struct "hil_state_msg"))
gcsTowerAux name istream ostream fm sens pos ctl motor = do
  streamrate <- channel
  datarate   <- channel
  hil        <- channelWithSize
  task ("gcsReceiveTask" ++ name) $
    gcsReceiveTask istream (src streamrate) (src datarate) (src hil)
  task ("gcsTransmitTask" ++ name) $
    gcsTransmitTask ostream (snk streamrate) (snk datarate) fm sens
      pos ctl motor
  addDepends HIL.hilStateModule
  return (snk hil)

