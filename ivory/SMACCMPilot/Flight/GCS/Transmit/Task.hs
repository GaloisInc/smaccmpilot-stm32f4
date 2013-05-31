{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Prelude hiding (last)
import Data.Monoid

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Transmit.USARTSender
import SMACCMPilot.Flight.GCS.Stream

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode as FM

sysid, compid :: Uint8
sysid = 1
compid = 0

gcsTransmitTask :: MemArea (Struct "usart")
                -> ChannelSink (Struct "gcsstream_timing")
                -> DataSink (Struct "flightmode")
                -> DataSink (Struct "sensors_result")
                -> DataSink (Struct "position_result")
                -> DataSink (Struct "controloutput")
                -> DataSink (Struct "servos")
                -> Task ()
gcsTransmitTask usart sp_sink fm_sink se_sink ps_sink ct_sink sr_sink = do
  streamPeriodRxer <- withChannelReceiver sp_sink  "streamperiods"
  fmReader         <- withDataReader fm_sink "flightmode"
  sensorsReader    <- withDataReader se_sink "sensors"
  posReader        <- withDataReader ps_sink "position"
  ctlReader        <- withDataReader ct_sink "control"
  servoReader      <- withDataReader sr_sink "servos"

  n <- freshname
  let (chan1, cmods) = messageDriver (usartSender usart n sysid compid)
  mapM_ withModule cmods
  withStackSize 512
  p <- withPeriod 50
  t <- withGetTimeMillis
  taskBody $ \schedule -> do
    initTime <- getTimeMillis t
    lastRun  <- local (ival initTime)

    s_periods     <- local (istruct [])
    s_schedule    <- local (istruct [])

    s_fm   <- local (istruct [])
    s_sens <- local (istruct [])
    s_pos  <- local (istruct [])
    s_ctl  <- local (istruct [])
    s_serv <- local (istruct [])

    let hstream = onChannel streamPeriodRxer $ \newperiods -> do
          now <- getTimeMillis t
          setNewPeriods newperiods s_periods s_schedule now

    let htimer = onTimer p $ \now -> do
          -- Handler for all streams - if due, run action, then update schedule
          let onStream :: Label "gcsstream_timing" (Stored Uint32)
                       -> Ivory eff () -> Ivory eff ()
              onStream selector action = do
                last <- deref lastRun
                due <- streamDue (constRef s_periods) (constRef s_schedule)
                         selector last now
                ifte due
                  (do action
                      setNextTime (constRef s_periods) s_schedule selector now)
                  (return ())

          onStream S.heartbeat $ do
            readData schedule fmReader s_fm
            call_ (sendHeartbeat chan1) s_fm

          onStream S.servo_output_raw $ do
            readData schedule servoReader s_serv
            readData schedule ctlReader s_ctl
            call_ (sendServoOutputRaw chan1) s_serv s_ctl

          onStream S.attitude $ do
            readData schedule sensorsReader s_sens
            call_ (sendAttitude chan1) s_sens

          onStream S.gps_raw_int $ do
            readData schedule posReader s_pos
            call_ (sendGpsRawInt chan1) s_pos

          onStream S.vfr_hud $ do
            readData schedule posReader s_pos
            readData schedule ctlReader s_ctl
            readData schedule sensorsReader s_sens
            call_ (sendVfrHud chan1) s_pos s_ctl s_sens

          onStream S.global_position_int $ do
            readData schedule posReader s_pos
            readData schedule sensorsReader s_sens
            call_ (sendGlobalPositionInt chan1) s_pos s_sens

          onStream S.params $ do
            -- XXX our whole story for params is broken
            return ()

          -- Keep track of last run for internal scheduler
          store lastRun now
    eventLoop schedule $ hstream <> htimer

  taskModuleDef $ \_sch -> do
    depend FM.flightModeTypeModule
    depend S.gcsStreamTimingTypeModule
    mapM_ depend cmods

