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

import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import           SMACCMPilot.Flight.GCS.Stream
import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode      as FM
import qualified SMACCMPilot.Flight.Types.DataRate        as D
import qualified SMACCMPilot.Flight.GCS.Commsec           as C

import qualified Ivory.HXStream                           as H

--------------------------------------------------------------------------------

-- Take a Mavlink packet, encrypt it, hxstream it, then send it to the uart
-- ISR.
-- processAndEmit :: (SingI m, SingI n)
--   => ChannelEmitter n (Stored Uint8) -- Emitter to uart
--   -> Ref s' (Array 128 (Stored Uint8)) -- commsec package
--   -> ConstRef s (Array m (Stored Uint8)) -- Message
--   -> Ivory (AllocEffects eff) ()
-- processAndEmit ostream uavPkg arrref = do
--   -- C.cpyToPkg arrref uavPkg
--   -- C.encrypt C.uavCtx uavPkg

--   -- call_ H.encode uavPkg hx
-- --  let pkg = constRef uavPkg
--   arrayMap $ \i -> --emit_ ostream (constRef hx ! i)

--     emit_ ostream (arrref ! i)
--------------------------------------------------------------------------------

gcsTransmitTask :: (SingI nn, SingI n, SingI m)
                => ChannelSource nn (Stored Uint8) -- Channel to UART
                -> ChannelSink n    (Struct "gcsstream_timing")
                -> ChannelSink m    (Struct "data_rate_state")
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

  -- XXX current issue: need a way to change usartSender to be defined in terms
  -- of the ChannelReceiver. This means it will depend on the Task
  -- tower_task_loop_ module, which generates the code for the emitter.
  -- msgDriver <- gcsTransmitDriver uavPkg

  uavPkg  <- taskLocal "uavPkg"
  seqNum  <- taskLocalInit "txseqNum" (ival 0)

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

  -- If the Mavlink receiver sends new data rate info, broadcast it.
  onChannel dr_sink "dataRate" $ \dr -> do
    d <- local (istruct [])
    refCopy d dr
    call_ mkSendDataRate d seqNum uavPkg

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

    let send = arrayMap $ \ix -> emit_ uartTx (constRef uavPkg ! ix)

    onStream S.heartbeat $ do
      readData fmReader s_fm
      call_ mkSendHeartbeat s_fm seqNum uavPkg
      send

    onStream S.servo_output_raw $ do
      readData motorReader s_motor
      readData ctlReader s_ctl
      call_ mkSendServoOutputRaw s_motor s_ctl seqNum uavPkg
      send

    onStream S.attitude $ do
      readData sensorsReader s_sens
      call_ mkSendAttitude s_sens seqNum uavPkg
      send

    onStream S.gps_raw_int $ do
      readData posReader s_pos
      call_ mkSendGpsRawInt s_pos seqNum uavPkg
      send

    onStream S.vfr_hud $ do
      readData posReader s_pos
      readData ctlReader s_ctl
      readData sensorsReader s_sens
      call_ mkSendVfrHud s_pos s_ctl s_sens seqNum uavPkg
      send

    onStream S.global_position_int $ do
      readData posReader s_pos
      readData sensorsReader s_sens
      call_ mkSendGlobalPositionInt s_pos s_sens seqNum uavPkg
      send

    onStream S.params $ do
      -- XXX our whole story for params is broken
      return ()

    -- Keep track of last run for internal scheduler
    store lastRun now

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend D.dataRateTypeModule
    depend S.gcsStreamTimingTypeModule
    depend C.commsecModule
    incl H.encode
    depend senderModules
--    defStruct (Proxy :: Proxy "hxstream_state")
