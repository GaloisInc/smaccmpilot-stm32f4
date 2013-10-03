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

import qualified SMACCMPilot.Flight.Commsec.Decrypt as Dec
import qualified SMACCMPilot.Flight.Commsec.Encrypt as Enc

import qualified SMACCMPilot.Flight.Datalink as D

--------------------------------------------------------------------------------

gcsTower :: (SingI n0, SingI n1)
         => String
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p ()
gcsTower name istream ostream fm sens pos ctl motor = do
  _ <- gcsTowerAux name istream ostream fm sens pos ctl motor
  return ()

--------------------------------------------------------------------------------

gcsTowerHil :: (SingI n0, SingI n1)
         => String
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
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

--------------------------------------------------------------------------------

gcsTowerAux :: (SingI n0, SingI n1)
         => String
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> DataSink (Struct "flightmode")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position_result")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> Tower p (ChannelSink 4 (Struct "hil_state_msg"))
gcsTowerAux name istream ostream fm sens pos ctl motor = do
  -- GCS TX and encrypt tasks
  (gcsTxToEncSrc, gcsTxToEncRcv) <- channel
  -- GCS RX and decrypt tasks
  (decToGcsRxSrc, decToGcsRxRcv) <- channel

  streamrate <- channel
  datarate   <- channel
  hil        <- channelWithSize

  (  hxToDecRcv -- from Hx to decrypter
   , encToHxSrc -- from encrypter to Hx
   -- XXX unused
   , _statiStream :: ChannelSink 1 (Struct "radio_stat")
   -- XXX unused
   , _infoiStream :: ChannelSink 1 (Struct "radio_info")
   ) <- D.datalink istream ostream -- also creates task

  -- Rx
  task "decryptTask" $ Dec.decryptTask hxToDecRcv decToGcsRxSrc
  task ("gcsReceiveTask" ++ name) $
    gcsReceiveTask decToGcsRxRcv (src streamrate) (src datarate) (src hil)

  -- TX
  task "encryptTask" $ Enc.encryptTask gcsTxToEncRcv encToHxSrc
  task ("gcsTransmitTask" ++ name) $
    gcsTransmitTask gcsTxToEncSrc (snk streamrate) (snk datarate) fm sens
      pos ctl motor

  addDepends HIL.hilStateModule
  return (snk hil)

--------------------------------------------------------------------------------
