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

import SMACCMPilot.Mavlink.Send (mavlinkSendWithWriter)

import qualified SMACCMPilot.Flight.GCS.Commsec as C
import qualified Ivory.HXStream as H

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


-- gcsTransmitDriver :: -- (SingI n)
--                   -- => ChannelSource n (Stored Uint8) -- 1024 bytes: UART driver
--                   Task p MessageDriver
gcsTransmitDriver uavPkg = do
  taskdep <- taskDependency
  txseq   <- taskLocalInit "txseq" (ival 0)
  name    <- freshname

  let mavData =
        MavlinkData { sysId      = sysid
                    , compId     = compid
                    , writerName = "mavlinksender" ++ name
                    , txSeqNum   = txseq
                    , writerDeps = taskdep
                    }
  -- let s = mavlinkSendWithWriter mavData uavPkg

  let (driver, mods) = messageDriver mavData uavPkg

  taskModuleDef (mapM_ depend mods)
  mapM_ withModule mods
  return driver

gcsTransmitTask :: (SingI nn, SingI n, SingI m)
                => ChannelSource nn (Stored Uint8) -- Channel to UART
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
  withStackSize 1024

  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  motorReader      <- withDataReader mo_sink "motors"

  uavPkg           <- taskLocal "uavPkg"
  uartTx           <- withChannelEmitter ostream "ostream"

  -- XXX current issue: need a way to change usartSender to be defined in terms
  -- of the ChannelReceiver. This means it will depend on the Task
  -- tower_task_loop_ module, which generates the code for the emitter.
  msgDriver <- gcsTransmitDriver uavPkg

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
    now <- getTimeMillis t
    setNewPeriods newperiods s_periods s_schedule now

  -- If the Mavlink receiver sends new data rate info, broadcast it.
  onChannel dr_sink "dataRate" $ \dr -> do
    d <- local (istruct [])
    refCopy d dr
    call_ (sendDataRate msgDriver) d

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

    let send = arrayMap $ \ix -> emit_ uartTx (constRef uavPkg ! ix)

    onStream S.heartbeat $ do
      readData fmReader s_fm
      call_ (sendHeartbeat msgDriver) s_fm
      send

    onStream S.servo_output_raw $ do
      readData motorReader s_motor
      readData ctlReader s_ctl
      call_ (sendServoOutputRaw msgDriver) s_motor s_ctl
      send

    onStream S.attitude $ do
      readData sensorsReader s_sens
      call_ (sendAttitude msgDriver) s_sens
      send

    onStream S.gps_raw_int $ do
      readData posReader s_pos
      call_ (sendGpsRawInt msgDriver) s_pos
      send

    onStream S.vfr_hud $ do
      readData posReader s_pos
      readData ctlReader s_ctl
      readData sensorsReader s_sens
      call_ (sendVfrHud msgDriver) s_pos s_ctl s_sens
      send

    onStream S.global_position_int $ do
      readData posReader s_pos
      readData sensorsReader s_sens
      call_ (sendGlobalPositionInt msgDriver) s_pos s_sens
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
--    defStruct (Proxy :: Proxy "hxstream_state")
