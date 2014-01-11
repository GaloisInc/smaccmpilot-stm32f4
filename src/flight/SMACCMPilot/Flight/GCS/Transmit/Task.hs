{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  , GCSTxRequires(..)
  ) where

import Prelude hiding (last)
import Data.Traversable (traverse)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import           SMACCMPilot.Mavlink.Send
import qualified SMACCMPilot.Communications               as C
import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import           SMACCMPilot.Flight.GCS.Stream
import           SMACCMPilot.Param

import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as T

import qualified SMACCMPilot.Mavlink.Messages.VehCommsec  as VC

--------------------------------------------------------------------------------

data GCSTxRequires n =
  GCSTxRequires
    { tx_ctl_law     :: DataSink (Struct "control_law")
    , tx_sens        :: DataSink (Struct "sensors_result")
    , tx_position    :: DataSink (Struct "position")
    , tx_ctl         :: DataSink (Struct "controloutput")
    , tx_motors      :: DataSink (Struct "motors")
    , tx_alt_ctl     :: DataSink (Struct "alt_control_dbg")
    , tx_att_ctl     :: DataSink (Struct "att_control_dbg")
    , tx_pos_ctl     :: DataSink (Struct "pos_control_dbg")
    , tx_radio_stat  :: DataSink (Struct "radio_stat")
    , tx_veh_commsec :: DataSink (Struct "veh_commsec_msg")
    , tx_mon_commsec :: DataSink (Stored IBool)
    , tx_param_req   :: ChannelSink n (Stored Sint16)
    }


gcsTransmitTask :: (SingI n0, SingI n1, SingI n2)
                => ChannelSource n0 C.MAVLinkArray -- Channel to encrypter
                -> ChannelSink   n1 (Struct "gcsstream_timing")
                -> [Param PortPair]
                -> GCSTxRequires n2
                -> Task p ()
gcsTransmitTask mavStream sp_sink params input = do
  withStackSize 1024

  clReader          <- withDataReader (tx_ctl_law     input) "controllaw"
  sensorsReader     <- withDataReader (tx_sens        input) "sensors"
  posReader         <- withDataReader (tx_position    input) "position"
  ctlReader         <- withDataReader (tx_ctl         input) "control"
  motorReader       <- withDataReader (tx_motors      input) "motors"
  radioReader       <- withDataReader (tx_radio_stat  input) "radio"
  altControlReader  <- withDataReader (tx_alt_ctl     input) "alt_control"
  attControlReader  <- withDataReader (tx_att_ctl     input) "att_control"
  posControlReader  <- withDataReader (tx_pos_ctl     input) "pos_control"
  commsecInfoReader <- withDataReader (tx_veh_commsec input) "commsecInfo"
  commMonitorReader <- withDataReader (tx_mon_commsec input) "commsecMonitor"

  mavTx             <- withChannelEmitter mavStream "gcsTxToEncSrc"

  -- the mavlink packet we're packing
  mavlinkStruct  <-
    taskLocal "mavlinkStruct" :: Task p (Ref Global (Struct "mavlinkPacket"))
  -- When we've filled up an array, we sent it.
  mavlinkSend    <-
    taskLocal "mavlinkSend" :: Task p (Ref Global C.MAVLinkArray)
  -- mavlink sequence numbers
  seqNum         <- taskLocalInit "txseqNum" (ival 0)

  t <- withGetTimeMillis

  paramReqs       <- withChannelReceiver (tx_param_req input) "paramReqs"
  read_params     <- traverse paramReader (map (fmap portPairSink) params)
  getParamInfo    <- makeGetParamInfo read_params

  lastRun     <- taskLocal "lastrun"
  s_periods   <- taskLocal "periods"
  s_schedule  <- taskLocal "schedule"
  -- Current index into the sending array
  mavSendIx   <- taskLocalInit "mav_send_ix" (ival (0 :: Sint32))

  -- Keep track of commsec info (it's a data port) so we don't resend.
  prevCommsecInfo <- taskLocalInit "prevCommsecInfo" (istruct [])

  taskInit $ do
    initTime <- getTimeMillis t
    store lastRun initTime

  onChannel sp_sink "streamPeriod" $ \newperiods ->
    setNewPeriods newperiods s_periods s_schedule =<< getTimeMillis t

  let processMav :: (Scope s ~ GetAlloc eff)
                 => T.GcsTimingLabel -> Ivory eff ()
      processMav selector = do
        let arr = mavlinkStruct ~> mav_array
        mavlen <- mavlinkStruct ~>* mav_size
        currIx <- deref mavSendIx
        let mavlen' = safeCast mavlen
        -- Do we have enough room to pack another packet?
        let room    = arrayLen mavlinkSend - currIx >=? mavlen'
        -- Is the message time-critical?
        hardRt     <- T.getDeadline selector s_periods
        let copyMav :: (Scope cs ~ GetAlloc eff) => Sint32 -> Ivory eff ()
            copyMav ix = do
              arrayCopy mavlinkSend arr ix mavlen'
              store mavSendIx (ix + mavlen')
          -- If there's room to pack another message and it's soft real-time,
          -- just pack.
        ifte_ (room .&& iNot (T.isHardRealTime hardRt))
          (copyMav currIx)
          -- Otherwise, emit and then store.
          (do emit_ mavTx (constRef mavlinkSend)
              -- Zero sending array and reset index
              arrayMap $ \ix -> store (mavlinkSend ! ix) 0
              copyMav 0)

  onPeriod 50 $ \now -> do

    -- Handler for all streams - if due, run action, then update schedule
    let onStream :: T.GcsTimingLabel
                 -> Ivory eff ()
                 -> Ivory eff ()
        onStream selector action = do
          due  <- streamDue (constRef s_periods) (constRef s_schedule)
                    selector now
          let arr = mavlinkStruct ~> mav_array
          when due $ do
            -- Zero out mavlink array first
            arrayMap $ \ix -> store (arr ! ix) 0
            action
            setNextTime (constRef s_periods) s_schedule selector now

    onStream T.heartbeat $ do
      l_cl    <- local (istruct [])
      cm      <- local (ival true)
      readData clReader l_cl
      readData commMonitorReader cm
      call_ mkSendHeartbeat l_cl seqNum mavlinkStruct =<< deref cm
      processMav T.heartbeat

    onStream T.servo_output_raw $ do
      l_motor <- local (istruct [])
      l_ctl   <- local (istruct [])
      readData motorReader l_motor
      readData ctlReader l_ctl
      call_ mkSendServoOutputRaw l_motor l_ctl seqNum mavlinkStruct
      processMav T.servo_output_raw

    onStream T.attitude $ do
      l_sens <- local (istruct [])
      readData sensorsReader l_sens
      call_ mkSendAttitude l_sens seqNum mavlinkStruct
      processMav T.attitude

    onStream T.gps_raw_int $ do
      l_pos <- local (istruct [])
      readData posReader l_pos
      call_ mkSendGpsRawInt l_pos seqNum mavlinkStruct
      processMav T.gps_raw_int

    onStream T.vfr_hud $ do
      l_pos  <- local (istruct [])
      l_ctl  <- local (istruct [])
      l_sens <- local (istruct [])
      readData posReader l_pos
      readData ctlReader l_ctl
      readData sensorsReader l_sens
      call_ mkSendVfrHud l_pos l_ctl l_sens seqNum mavlinkStruct
      processMav T.vfr_hud

      -- piggyback controller debug here
      l_alt_ctl <- local (istruct [])
      readData altControlReader l_alt_ctl
      call_ mkSendAltCtlDebug (constRef l_alt_ctl) seqNum mavlinkStruct
      l_att_ctl <- local (istruct [])
      readData attControlReader l_att_ctl
      call_ mkSendAttCtlDebug (constRef l_att_ctl) seqNum mavlinkStruct
      l_pos_ctl <- local (istruct [])
      readData posControlReader l_pos_ctl
      call_ mkSendPosCtlDebug (constRef l_pos_ctl) seqNum mavlinkStruct

      processMav T.vfr_hud

    onStream T.global_position_int $ do
      l_pos  <- local (istruct [])
      l_sens <- local (istruct [])
      readData posReader l_pos
      readData sensorsReader l_sens
      call_ mkSendGlobalPositionInt l_pos l_sens now seqNum mavlinkStruct
      processMav T.global_position_int

    onStream T.params $ do
      x       <- local (ival 0)
      success <- receive paramReqs x
      when success $ do
        ix    <- deref x
        msg   <- local (istruct [])
        found <- call (paramInfoGetter getParamInfo) ix msg
        when found $ do
          call_ mkSendParamValue msg seqNum mavlinkStruct
          processMav T.params

    onStream T.veh_commsec $ do
      commInfo <- local (istruct [])
      readData commsecInfoReader commInfo

      -- See if we got new info on commsec errors before sending it out.  The
      -- commsec reporting is a dataport, so it may not have been updated.  (A
      -- dataport ensures we have the latest info.)
      let comp :: forall eff t . (IvoryEq t, IvoryVar t)
               => Label "veh_commsec_msg" (Stored t) -> Ivory eff IBool
          comp field = do
            f0 <- commInfo ~>* field
            f1 <- prevCommsecInfo ~>* field
            return (f0 ==? f1)
      eq <- return . foldl (.&&) true =<< sequence
              [ comp VC.time
              , comp VC.good_msgs
              , comp VC.bad_msgs
              , comp VC.commsec_err
              ]
      unless eq $ do
        refCopy prevCommsecInfo commInfo
        call_ mkSendVehCommsec (constRef commInfo) seqNum mavlinkStruct
        processMav T.veh_commsec

    onStream T.radio $ do
      l_radio <- local (istruct [])
      readData radioReader l_radio
      call_ mkSendRadio l_radio seqNum mavlinkStruct
      processMav T.radio

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    mapM_ depend stdlibModules
    depend CL.controlLawTypeModule
    depend T.gcsStreamTimingTypeModule
    depend senderModules
    depend paramModule
    depend mavlinkSendModule

