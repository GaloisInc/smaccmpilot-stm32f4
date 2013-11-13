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

import           SMACCMPilot.Mavlink.Send
import           SMACCMPilot.Param
import qualified SMACCMPilot.Communications               as C
import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import           SMACCMPilot.Flight.GCS.Stream
import           SMACCMPilot.Flight.Control.AltHold
import           SMACCMPilot.Param

import qualified SMACCMPilot.Flight.Types.Armed           as A
import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as T
import qualified SMACCMPilot.Flight.Types.FlightMode      as FM

--------------------------------------------------------------------------------

gcsTransmitTask :: (SingI n0, SingI n1, SingI n2, SingI n3)
                => ChannelSource n0 C.MAVLinkArray -- Channel to encrypter
                -> ChannelSink   n1 (Struct "gcsstream_timing")
                -> ChannelSink   n2 (Struct "data_rate_state")
                -> DataSink         (Struct "flightmode")
                -> DataSink         (Stored A.ArmedMode)
                -> DataSink         (Struct "sensors_result")
                -> DataSink         (Struct "position")
                -> DataSink         (Struct "controloutput")
                -> DataSink         (Struct "motors")
                -> DataSink         (Struct "radio_stat")
                -> DataSink         (Struct "alt_hold_state")
                -> ChannelSink n3   (Stored Sint16)
                -> [Param PortPair]
                -> Task p ()
gcsTransmitTask mavStream sp_sink _dr_sink fm_sink armed_sink se_sink ps_sink
                ct_sink mo_sink ra_sink ah_sink param_req_sink params
  = do
  withStackSize 1024

  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  motorReader      <- withDataReader mo_sink "motors"
  radioReader      <- withDataReader ra_sink "radio"
  armedReader      <- withDataReader armed_sink "armed"
  altHoldReader    <- withDataReader ah_sink "alt_hold"
  mavTx            <- withChannelEmitter mavStream "gcsTxToEncSrc"

  -- the mavlink packet we're packing
  mavlinkStruct  <-
    taskLocal "mavlinkStruct" :: Task p (Ref Global (Struct "mavlinkPacket"))
  -- When we've filled up an array, we sent it.
  mavlinkSend    <-
    taskLocal "mavlinkSend" :: Task p (Ref Global C.MAVLinkArray)
  -- mavlink sequence numbers
  seqNum         <- taskLocalInit "txseqNum" (ival 0)

  t <- withGetTimeMillis

  paramReqs       <- withChannelReceiver param_req_sink "paramReqs"
  read_params     <- traverse paramReader (map (fmap portPairSink) params)
  getParamInfo    <- makeGetParamInfo read_params

  lastRun     <- taskLocal "lastrun"
  s_periods   <- taskLocal "periods"
  s_schedule  <- taskLocal "schedule"
  -- Current index into the sending array
  mavSendIx   <- taskLocalInit "mav_send_ix" (ival (0 :: Sint32))

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
      l_fm <- local (istruct [])
      l_armed <- local izero
      readData fmReader l_fm
      readData armedReader l_armed
      l <- deref l_armed
      b <- local izero
      store b (l ==? A.as_ARMED)
      call_ mkSendHeartbeat l_fm b seqNum mavlinkStruct
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

      -- piggyback alt hold state debug here
      l_alt_hold <- local (istruct [])
      readData altHoldReader l_alt_hold
      call_ mkSendAltHoldDebug (constRef l_alt_hold) seqNum mavlinkStruct
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

    onStream T.radio $ do
      l_radio <- local (istruct [])
      readData radioReader l_radio
      call_ mkSendRadio l_radio seqNum mavlinkStruct
      processMav T.radio

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    mapM_ depend stdlibModules
    depend FM.flightModeTypeModule
    depend T.gcsStreamTimingTypeModule
    depend senderModules
    depend paramModule
    depend mavlinkSendModule
    depend altHoldModule
