{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower
  ( gcsTower
  , gcsTowerHil
  ) where

import Control.Monad (void)

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib (stdlibModules)

import qualified SMACCMPilot.Mavlink.Messages.HilState     as HIL

import SMACCMPilot.Flight.GCS.HIL
import SMACCMPilot.Param
import SMACCMPilot.Flight.GCS.Transmit.Task
import SMACCMPilot.Flight.GCS.Receive.Task

import qualified SMACCMPilot.Flight.Commsec.Decrypt      as Dec
import qualified SMACCMPilot.Flight.Commsec.Encrypt      as Enc

import qualified SMACCMPilot.Flight.Datalink             as D
import qualified SMACCMPilot.Flight.Commsec.CommsecOpts  as C

--------------------------------------------------------------------------------

gcsTower :: (SingI n0, SingI n1, SingI n2, SingI n3)
         => String
         -> C.Options
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> DataSink   (Struct "control_law")
         -> ChannelSource n2 (Struct "control_law_request")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> ChannelSource n3 (Struct "rc_channels_override_msg")
         -> DataSink (Struct "alt_control_dbg")
         -> DataSink (Struct "att_control_dbg")
         -> [Param PortPair]
         -> Tower p ()
gcsTower name opts istream ostream cl_state ctl_req sens
         pos ctl motor rc_ovr alt_snk att_snk params
  =
  void $ gcsTowerAux name opts istream ostream cl_state ctl_req
           sens pos ctl motor rc_ovr alt_snk att_snk params

--------------------------------------------------------------------------------

gcsTowerHil :: (SingI n0, SingI n1, SingI n2, SingI n3)
         => String
         -> C.Options
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> DataSink   (Struct "control_law")
         -> ChannelSource n2 (Struct "control_law_request")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> ( DataSource (Struct "sensors_result")
            , DataSink   (Struct "sensors_result"))
         -> ChannelSource n3 (Struct "rc_channels_override_msg")
         -> DataSink (Struct "alt_control_dbg")
         -> DataSink (Struct "att_control_dbg")
         -> [Param PortPair]
         -> Tower p ()
gcsTowerHil name opts istream ostream cl_state ctl_req
            ctl motor sensors rc_ovr alt_snk att_snk params
  = do
  let sensors_state = snk sensors
  position      <- dataport
  hil           <-
    gcs cl_state ctl_req sensors_state (snk position) ctl
      motor rc_ovr alt_snk att_snk params
  task "hilTranslator" $ hilTranslator hil (src sensors) (src position)
  where
  gcs = gcsTowerAux name opts istream ostream

--------------------------------------------------------------------------------

gcsTowerAux :: (SingI n0, SingI n1, SingI n2, SingI n3)
         => String
         -> C.Options
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> DataSink   (Struct "control_law")
         -> ChannelSource n2 (Struct "control_law_request")
         -> DataSink (Struct "sensors_result")
         -> DataSink (Struct "position")
         -> DataSink (Struct "controloutput")
         -> DataSink (Struct "motors")
         -> ChannelSource n3 (Struct "rc_channels_override_msg")
         -> DataSink (Struct "alt_control_dbg")
         -> DataSink (Struct "att_control_dbg")
         -> [Param PortPair]
         -> Tower p (ChannelSink 4 (Struct "hil_state_msg"))
gcsTowerAux name opts istream ostream cl_state ctl_req sens pos
            ctl motor rc_ovr alt_snk att_snk params
  = do
  -- GCS TX and encrypt tasks
  (gcsTxToEncSrc, gcsTxToEncRcv) <- channel
  -- GCS RX and decrypt tasks
  (decToGcsRxSrc, decToGcsRxRcv) <- channel

  streamrate <- channel
  -- XXX hack to make sure we can send all parameters on "fetch"
  param_req  <- channelWithSize :: Tower p ( ChannelSource 512 (Stored Sint16)
                                           , ChannelSink   512 (Stored Sint16))
  hil        <- channelWithSize

  (  hxToDecRcv -- from Hx to decrypter
   , encToHxSrc -- from encrypter to Hx
   , radioStatStream :: ChannelSink 2 (Struct "radio_stat")
   -- XXX unused
   , _infoiStream :: ChannelSink 2 (Struct "radio_info")
   ) <- D.datalink name istream ostream -- also creates task

  radioStat <- stateProxy ("radio_status_" ++ name) radioStatStream

  -- Rx
  task (named "decryptTask") $ Dec.decryptTask opts hxToDecRcv decToGcsRxSrc
  task (named "gcsReceiveTask") $
    gcsReceiveTask decToGcsRxRcv (src streamrate) (src hil)
      ctl_req (src param_req) rc_ovr params

  -- TX
  task (named "encryptTask") $ Enc.encryptTask opts gcsTxToEncRcv encToHxSrc
  task (named "gcsTransmitTask") $
    gcsTransmitTask gcsTxToEncSrc (snk streamrate) cl_state
      sens pos ctl motor radioStat alt_snk att_snk (snk param_req) params
  addDepends HIL.hilStateModule
  mapM_ addDepends stdlibModules
  return (snk hil)
  where named n = n ++ "_" ++ name
--------------------------------------------------------------------------------
