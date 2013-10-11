{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Prelude hiding (last)
import Data.Traversable (traverse)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import           SMACCMPilot.Flight.GCS.Stream
import           SMACCMPilot.Param
import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as T
import qualified SMACCMPilot.Flight.Types.FlightMode      as FM
import qualified SMACCMPilot.Flight.Types.DataRate        as D
import qualified SMACCMPilot.Communications               as C

--------------------------------------------------------------------------------

gcsTransmitTask :: (SingI n0, SingI n1, SingI n2, SingI n3)
                => ChannelSource n0 C.MAVLinkArray -- Channel to encrypter
                -> ChannelSink   n1 (Struct "gcsstream_timing")
                -> ChannelSink   n2 (Struct "data_rate_state")
                -> DataSink         (Struct "flightmode")
                -> DataSink         (Struct "sensors_result")
                -> DataSink         (Struct "position")
                -> DataSink         (Struct "controloutput")
                -> DataSink         (Struct "motors")
                -> DataSink         (Struct "radio_stat")
                -> ChannelSink n3   (Stored Sint16)
                -> [Param PortPair]
                -> Task p ()
gcsTransmitTask mavStream sp_sink _dr_sink fm_sink se_sink ps_sink ct_sink
                mo_sink ra_sink param_req_sink params
  = do
  withStackSize 1024

  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  motorReader      <- withDataReader mo_sink "motors"
  radioReader      <- withDataReader ra_sink "radio"
  mavTx            <- withChannelEmitter mavStream "gcsTxToEncSrc"

  mavlinkPacket  <- taskLocal "mavlinkPacket"
  -- mavlink sequence numbers
  seqNum         <- taskLocalInit "txseqNum" (ival 0)

  t <- withGetTimeMillis

  paramReqs       <- withChannelReceiver param_req_sink "paramReqs"
  read_params     <- traverse paramReader (map (fmap portPairSink) params)
  getParamInfo    <- makeGetParamInfo read_params

  lastRun    <- taskLocal "lastrun"
  s_periods  <- taskLocal "periods"
  s_schedule <- taskLocal "schedule"
  s_fm       <- taskLocal "flightmode"
  s_sens     <- taskLocal "sensors"
  s_pos      <- taskLocal "position"
  s_ctl      <- taskLocal "control"
  s_motor    <- taskLocal "motor"

  taskInit $ do
    initTime <- getTimeMillis t
    store lastRun initTime

  onChannel sp_sink "streamPeriod" $ \newperiods -> do
    setNewPeriods newperiods s_periods s_schedule =<< getTimeMillis t

  -- XXX
  -- If the Mavlink receiver sends new data rate info, broadcast it.
  -- onChannel dr_sink "dataRate" $ \dr -> do
  --   d <- local (istruct [])
  --   refCopy d dr
  --   call_ mkSendDataRate d seqNum mavlinkPacket
  --   call_ processStream mavlinkPacket

  let processMav :: (Scope s ~ GetAlloc eff) => Ivory eff ()
      processMav = emit_ mavTx (constRef mavlinkPacket)

  onPeriod 50 $ \now -> do

    -- Handler for all streams - if due, run action, then update schedule
    let onStream :: Label "gcsstream_timing" (Stored Uint32)
                 -> Ivory eff () -> Ivory eff ()
        onStream selector action = do
          last <- deref lastRun
          due  <- streamDue (constRef s_periods) (constRef s_schedule)
                    selector last now
          when due $ do
            action
            setNextTime (constRef s_periods) s_schedule selector now

    onStream T.heartbeat $ do
      readData fmReader s_fm
      call_ mkSendHeartbeat s_fm seqNum mavlinkPacket
      processMav

    onStream T.servo_output_raw $ do
      readData motorReader s_motor
      readData ctlReader s_ctl
      call_ mkSendServoOutputRaw s_motor s_ctl seqNum mavlinkPacket
      processMav

    onStream T.attitude $ do
      readData sensorsReader s_sens
      call_ mkSendAttitude s_sens seqNum mavlinkPacket
      processMav

    onStream T.gps_raw_int $ do
      readData posReader s_pos
      call_ mkSendGpsRawInt s_pos seqNum mavlinkPacket
      processMav

    onStream T.vfr_hud $ do
      readData posReader s_pos
      readData ctlReader s_ctl
      readData sensorsReader s_sens
      call_ mkSendVfrHud s_pos s_ctl s_sens seqNum mavlinkPacket
      processMav

    onStream T.global_position_int $ do
      readData posReader s_pos
      readData sensorsReader s_sens
      call_ mkSendGlobalPositionInt s_pos s_sens seqNum mavlinkPacket
      processMav

    onStream T.params $ do
      x       <- local (ival 0)
      success <- receive paramReqs x
      when success $ do
        ix    <- deref x
        msg   <- local (istruct [])
        found <- call getParamInfo ix msg
        when found $ do
          call_ mkSendParamValue msg seqNum mavlinkPacket
          processMav

    onStream T.radio $ do
      l_radio <- local (istruct [])
      readData radioReader l_radio
      call_ mkSendRadio l_radio seqNum mavlinkPacket
      processMav

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    mapM_ depend stdlibModules
    depend FM.flightModeTypeModule
    depend D.dataRateTypeModule
    depend T.gcsStreamTimingTypeModule
    depend senderModules
    incl getParamInfo
