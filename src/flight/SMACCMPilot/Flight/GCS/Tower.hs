{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.Tower
  ( gcsTower
  , gcsTowerHil
  , GCSRequires(..)
  , GCSProvides(..)
  , HILRequires(..)
  ) where

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

data GCSRequires =
  GCSRequires
    { gcs_ctl_law_in  :: DataSink (Struct "control_law")
    , gcs_sens_in     :: DataSink (Struct "sensors_result")
    , gcs_position_in :: DataSink (Struct "position")
    , gcs_ctl_in      :: DataSink (Struct "controloutput")
    , gcs_motors_in   :: DataSink (Struct "motors")
    , gcs_alt_ctl_in  :: DataSink (Struct "alt_control_dbg")
    , gcs_att_ctl_in  :: DataSink (Struct "att_control_dbg")
    , gcs_pos_ctl_in  :: DataSink (Struct "pos_control_dbg")
    , gcs_commsec_in  :: DataSink (Struct "veh_commsec_msg")
    }

data GCSProvides =
  GCSProvides
    { gcs_ctl_law_req  :: ChannelSource 16 (Struct "control_law_request")
    , gcs_rc_override  :: ChannelSource 16 (Struct "rc_channels_override_msg")
    , gcs_commsec_info :: DataSource (Struct "veh_commsec_msg")
    , gcs_hil_state    :: Maybe (ChannelSource 4  (Struct "hil_state_msg"))
    }

data HILRequires =
  HILRequires
    { hil_sensors_in  :: DataSource (Struct "sensors_result")
    , hil_position_in :: DataSource (Struct "position")
    }

gcsTower :: (SingI n0, SingI n1)
         => String
         -> C.Options
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> GCSRequires
         -> GCSProvides
         -> [Param PortPair]
         -> Tower p ()
gcsTower name opts istream ostream req prov params
  = do
  streamrate <- channel
  -- XXX hack to make sure we can send all parameters on "fetch"
  param_req  <- channelWithSize :: Tower p
    ( ChannelSource 512 (Stored Sint16) , ChannelSink   512 (Stored Sint16))
  (  rx_encrypted -- from Hx to decrypter
   , tx_encrypted -- from encrypter to Hx
   , radio_stat_snk :: ChannelSink 2 (Struct "radio_stat")
   , _radio_info_snk :: ChannelSink 2 (Struct "radio_info") -- XXX fixme
   ) <- D.datalink name istream ostream -- also creates task

  radio_stat <- stateProxy ("radio_status_" ++ name) radio_stat_snk

  rx_plaintext <- channel
  task (named "decryptTask") $
    Dec.decryptTask opts rx_encrypted (src rx_plaintext)
      (gcs_commsec_info prov)
  task (named "gcsReceiveTask") $
    gcsReceiveTask (snk rx_plaintext) (src streamrate) params
      GCSRxRequires
        { rx_hil         = gcs_hil_state   prov
        , rx_ctl_req     = gcs_ctl_law_req prov
        , rx_param_req   = src param_req
        , rx_rc_override = gcs_rc_override prov
        }

  tx_plaintext <- channel
  task (named "encryptTask") $ Enc.encryptTask opts (snk tx_plaintext) tx_encrypted
  task (named "gcsTransmitTask") $
    gcsTransmitTask (src tx_plaintext) (snk streamrate) params
      GCSTxRequires
         { tx_ctl_law     = gcs_ctl_law_in  req
         , tx_sens        = gcs_sens_in     req
         , tx_position    = gcs_position_in req
         , tx_ctl         = gcs_ctl_in      req
         , tx_motors      = gcs_motors_in   req
         , tx_alt_ctl     = gcs_alt_ctl_in  req
         , tx_att_ctl     = gcs_att_ctl_in  req
         , tx_pos_ctl     = gcs_pos_ctl_in  req
         , tx_veh_commsec = gcs_commsec_in  req
         , tx_param_req   = snk param_req
         , tx_radio_stat  = radio_stat
         }

  addDepends HIL.hilStateModule
  mapM_ addDepends stdlibModules
  where named n = n ++ "_" ++ name

--------------------------------------------------------------------------------

gcsTowerHil :: (SingI n0, SingI n1)
         => String
         -> C.Options
         -> ChannelSink   n0 (Stored Uint8)
         -> ChannelSource n1 (Stored Uint8)
         -> GCSRequires
         -> GCSProvides
         -> HILRequires
         -> [Param PortPair]
         -> Tower p ()
gcsTowerHil name opts istream ostream gcs_req gcs_prov hil_req params = do
  hil_ch <- channelWithSize
  let hil_prov = gcs_prov { gcs_hil_state = Just (src hil_ch) }
  gcsTower name opts istream ostream gcs_req hil_prov params
  task "hilTranslator" $ hilTranslator (snk hil_ch)
                                       (hil_sensors_in hil_req)
                                       (hil_position_in hil_req)
