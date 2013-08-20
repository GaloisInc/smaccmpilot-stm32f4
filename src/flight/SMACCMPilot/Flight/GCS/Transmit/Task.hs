{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Stdlib (when)
import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Stream

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode      as FM
import qualified SMACCMPilot.Flight.Types.DataRate        as D

import SMACCMPilot.Mavlink.Send (mavlinkSendWithWriter, MavlinkWriteMacro(..))

sysid, compid :: Uint8
sysid = 1
compid = 0

gcsTransmitDriver :: (SingI n)
                  => ChannelSource n (Stored Uint8)
                  -> Task p MessageDriver
gcsTransmitDriver chan = do
  ostream <- withChannelEmitter chan "ostream"
  taskdep <- taskDependency
  txseq   <- taskLocalInit "txseq" (ival 0)
  name <- freshname
  let w :: (SingI m)
        => ConstRef s (Array m (Stored Uint8))
        -> Ivory (AllocEffects cs) ()
      w arrref = arrayMap $ \i ->
          emit_ ostream (arrref ! i)
      s = mkSender name txseq (MavlinkWriteMacro w) taskdep
      (driver, ms) = messageDriver s
  taskModuleDef $ mapM_ depend ms
  mapM_ withModule ms
  return driver
  where
  mkSender n txseq writer deps =
    mavlinkSendWithWriter sysid compid ("mavlinksender" ++ n) txseq writer deps

gcsTransmitTask :: (SingI nn, SingI n, SingI m)
                => ChannelSource nn (Stored Uint8)
                -> ChannelSink n (Struct "gcsstream_timing")
                -> ChannelSink m (Struct "data_rate_state")
                -> DataSink (Struct "flightmode")
                -> DataSink (Struct "sensors_result")
                -> DataSink (Struct "position_result")
                -> DataSink (Struct "controloutput")
                -> DataSink (Struct "motors")
                -> Task p ()
gcsTransmitTask ostream sp_sink dr_sink fm_sink se_sink ps_sink ct_sink mo_sink
  = do
  streamPeriodRxer <- withChannelReceiver sp_sink  "streamperiods"
  drRxer           <- withChannelReceiver dr_sink  "data_rate"
  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  motorReader      <- withDataReader mo_sink "motors"

  -- XXX current issue: need a way to change usartSender to be defined in terms
  -- of the ChannelReceiver. This means it will depend on the Task
  -- tower_task_loop_ module, which generates the code for the emitter.
  chan1 <- gcsTransmitDriver ostream
  withStackSize 512
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

  onChannel streamPeriodRxer $ \newperiods -> do
    now <- getTimeMillis t
    setNewPeriods newperiods s_periods s_schedule now

  -- If the Mavlink receiver sends new data rate info, broadcast it.
  onChannel drRxer $ \dr -> do
    d <- local (istruct [])
    refCopy d dr
    call_ (sendDataRate chan1) d

  onPeriod 50 $ \now -> do
    -- Handler for all streams - if due, run action, then update schedule
    let onStream :: Label "gcsstream_timing" (Stored Uint32)
                 -> Ivory eff () -> Ivory eff ()
        onStream selector action = do
          last <- deref lastRun
          due <- streamDue (constRef s_periods) (constRef s_schedule)
                   selector last now
          when due $ do
            action
            setNextTime (constRef s_periods) s_schedule selector now

    onStream S.heartbeat $ do
      readData fmReader s_fm
      call_ (sendHeartbeat chan1) s_fm

    onStream S.servo_output_raw $ do
      readData motorReader s_motor
      readData ctlReader s_ctl
      call_ (sendServoOutputRaw chan1) s_motor s_ctl

    onStream S.attitude $ do
      readData sensorsReader s_sens
      call_ (sendAttitude chan1) s_sens

    onStream S.gps_raw_int $ do
      readData posReader s_pos
      call_ (sendGpsRawInt chan1) s_pos

    onStream S.vfr_hud $ do
      readData posReader s_pos
      readData ctlReader s_ctl
      readData sensorsReader s_sens
      call_ (sendVfrHud chan1) s_pos s_ctl s_sens

    onStream S.global_position_int $ do
      readData posReader s_pos
      readData sensorsReader s_sens
      call_ (sendGlobalPositionInt chan1) s_pos s_sens

    onStream S.params $ do
      -- XXX our whole story for params is broken
      return ()

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend D.dataRateTypeModule
    depend S.gcsStreamTimingTypeModule

