{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import           SMACCMPilot.Flight.GCS.Stream
import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as T
import qualified SMACCMPilot.Flight.Types.FlightMode      as FM
import qualified SMACCMPilot.Flight.Types.DataRate        as D
import qualified SMACCMPilot.Flight.GCS.Commsec           as C

import qualified Ivory.HXStream                           as H
import qualified SMACCMPilot.Shared                       as S

--------------------------------------------------------------------------------

processHx :: SingI n
          => ChannelEmitter n (Stored Uint8)
          -> Def ('[ Ref Global S.MavLinkArray
                   ] :-> ())
processHx uartTx = proc "processHx" $ \mavMsg -> body $ do
  pkg        <- local (iarray [] :: Init S.CommsecArray)
  C.copyToPkg (constRef mavMsg) pkg
  C.encrypt C.uavCtx pkg

  hxArr      <- local (iarray [] :: Init S.HxstreamArray)
  call_ H.encode 0 pkg hxArr
  arrayMap $ \ix -> emit_ uartTx (constRef hxArr ! ix)

--------------------------------------------------------------------------------

gcsTransmitTask :: (SingI n0, SingI n1, SingI n2)
                => ChannelSource n0 (Stored Uint8) -- Channel to UART
                -> ChannelSink n1   (Struct "gcsstream_timing")
                -> ChannelSink n2   (Struct "data_rate_state")
                -> DataSink         (Struct "flightmode")
                -> DataSink         (Struct "sensors_result")
                -> DataSink         (Struct "position_result")
                -> DataSink         (Struct "controloutput")
                -> DataSink         (Struct "motors")
                -> Task p ()
gcsTransmitTask ostream sp_sink dr_sink fm_sink se_sink ps_sink ct_sink mo_sink
  = do
  withStackSize 1024

  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  motorReader      <- withDataReader mo_sink "motors"
  uartTx           <- withChannelEmitter ostream "ostream"

  let processStream = processHx uartTx

  -- XXX current issue: need a way to change usartSender to be defined in terms
  -- of the ChannelReceiver. This means it will depend on the Task
  -- tower_task_loop_ module, which generates the code for the emitter.
  -- msgDriver <- gcsTransmitDriver uavPkg

  mavlinkPacket  <- taskLocal "mavlinkPacket"
  -- mavlink sequence numbers
  seqNum         <- taskLocalInit "txseqNum" (ival 0)

  t <- withGetTimeMillis

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
    C.setupCommsec

  onChannel sp_sink "streamPeriod" $ \newperiods -> do
    setNewPeriods newperiods s_periods s_schedule =<< getTimeMillis t

  -- XXX
  -- If the Mavlink receiver sends new data rate info, broadcast it.
  -- onChannel dr_sink "dataRate" $ \dr -> do
  --   d <- local (istruct [])
  --   refCopy d dr
  --   call_ mkSendDataRate d seqNum mavlinkPacket
  --   call_ processStream mavlinkPacket

  let processMav = call_ processStream mavlinkPacket

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
      -- XXX our whole story for params is broken
      return ()

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend D.dataRateTypeModule
    depend T.gcsStreamTimingTypeModule
    depend senderModules
    incl processStream
    depend C.commsecModule
    depend H.hxstreamModule
